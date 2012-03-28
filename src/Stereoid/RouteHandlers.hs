{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Stereoid.RouteHandlers where

import Control.Applicative (Applicative, Alternative, (<$>))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad (MonadPlus, mplus)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader (MonadReader, ReaderT(..), ask)
import Happstack.Server ( HasRqData, WebMonad, FilterMonad, ServerMonad, Happstack, mapServerPartT)
import Data.Ord (comparing)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Control.Monad (msum, liftM)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Acid
import Data.Acid.Custom
import Data.Aeson
import System.FilePath ((</>))
import LastFM.Request
import System.Process
import System.Exit  ( ExitCode(..) )
import Control.Applicative (optional)
import Control.Monad.Trans     (MonadIO(..), liftIO)
import Happstack.Server        (port      , Response     , ServerPartT, ok           , toResponse
                               ,simpleHTTP, nullConf     , seeOther   , dir          , notFound
                               ,seeOther  , asContentType, serveFile  , ToMessage(..), look      
                               ,forbidden , queryString  , decodeBody , defaultBodyPolicy
                               ,BodyPolicy(..),method,lookCookieValue,unauthorized,badRequest                                             )
import Happstack.Server.FileServe.BuildingBlocks (strictByteStringResponse, serveFileUsing)
import Happstack.Server.RqData (RqData, lookRead, getDataFn)
import qualified Happstack.Server.Cookie as Cookie
import Happstack.Server.Modified (filePathSendAllowRange)
import Happstack.Server.Internal.Types (Request(..),Method(..))
import Happstack.Server.Monads (askRq)
import Control.Concurrent (forkIO)
import Web.Routes.Boomerang hiding ((</>))
import Web.Routes              ( PathInfo(..), RouteT    , showURL , runRouteT
                               , Site(..)    , setDefault, mkSitePI, liftRouteT           )
import Stereoid.Jobs
import Stereoid.Auth
import Stereoid.Routing
import Stereoid.HTTPClient
import Stereoid.Persistence
import Stereoid.JsonInstances
import Stereoid.Types
import qualified Stereoid.Types.Internal as I

decodePolicy :: BodyPolicy
decodePolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)


site :: AcidState StereoidDb -> AcidState UserMap -> AcidState SessionMap -> String -> String -> Site Sitemap (App Response)
site sdb users sessions resourcedir statedir = setDefault Home $ boomerangSite (runRouteT (route sdb users sessions resourcedir statedir)) sitemap

route :: AcidState StereoidDb -> AcidState UserMap -> AcidState SessionMap -> String -> String -> Sitemap -> RouteT Sitemap App Response
route sdb users sessions resourcedir statedir url =
     do decodeBody decodePolicy
        rq <- askRq
        let chk = checkToken sessions 15
--        let meth = rqMethod rq
        case rqMethod rq of
             GET -> case url of
                    (Stream songId)         -> serveSong sdb songId
                    (AlbumInfo albumId)     -> chk $ albumData sdb albumId
                    (ArtistInfo artistId)   -> chk $ artistData sdb artistId
                    (ArtistAlbums artistId) -> chk $ artistAlbums sdb artistId
                    (Songs)                 -> chk $ songsAll sdb 
                    (Artists)               -> chk $ artistsAll sdb 
                    (Albums)                -> chk $ albumsAll sdb 
                    (AlbumSongs albumId)    -> chk $ albumSongs sdb albumId
                    (AlbumM3U albumId)      -> chk $ albumSongsM3U sdb albumId
                    (AlbumArt albumId)      -> chk $ serveArt sdb resourcedir statedir albumId
                    (AlbumArtThumb albumId) -> chk $ serveGenThumb sdb resourcedir statedir albumId
                    (Jobs)                  -> chk $ jobsAll sdb
                    (JobInfo jobId)         -> chk $ jobData sdb jobId
                    (Catalogs)              -> chk $ catalogsAll sdb
                    (CatalogInfo catalogId) -> chk $ catalogData sdb catalogId
             
             PUT -> case url of
                   (Sessions)              -> chk $ ok (toResponse "Session extended.")
                   (CatalogInfo catalogId) -> chk $ startJob sdb
             
             POST -> case url of
                    (Sessions)              -> authorize users sessions
                    (AlbumArt albumId)      -> chk $ getArtFromUrl sdb albumId
                    (Users)                 -> addUser users
                    (Catalogs)              -> chk $ catalogNew sdb
                    

data Acid = Acid { acidStereoidDb    :: AcidState StereoidDb
                 , acidUserMap      :: AcidState UserMap
                 , acidSessionMap   :: AcidState SessionMap
                 }

withAcid :: Maybe FilePath -> (Acid -> IO a) -> IO a
withAcid mBasePath action =
    let basePath = fromMaybe "_state" mBasePath
    in withLocalState (Just $ basePath </> "users") (UserMap emptyMap) $ \u ->
       withLocalState (Just $ basePath </> "sessions") (SessionMap emptyMap) $ \s ->
       withLocalState (Just $ basePath </> "stereoiddb") (sdbEmpty) $ \t ->
           action (Acid t u s )

newtype App a = App { unApp :: ServerPartT (ReaderT Acid IO) a }
    deriving ( Functor, Alternative, Applicative, Monad, MonadPlus, MonadIO
               , HasRqData, ServerMonad, WebMonad Response, FilterMonad Response
               , Happstack, MonadReader Acid)

instance (HasAcidState m a) => HasAcidState (RouteT url m) a where
    getAcidState = liftRouteT getAcidState

instance HasAcidState App StereoidDb where
    getAcidState = acidStereoidDb    <$> ask

instance HasAcidState App UserMap where
    getAcidState = acidUserMap      <$> ask

instance HasAcidState App SessionMap where
    getAcidState = acidSessionMap   <$> ask

runApp :: Acid -> App a -> ServerPartT IO a
runApp acid (App sp)= mapServerPartT (flip runReaderT acid) sp

catalogNew :: AcidState StereoidDb -> RouteT Sitemap App Response
catalogNew sdb = undefined

catalogsAll :: AcidState StereoidDb -> RouteT Sitemap App Response
catalogsAll sdb = undefined

catalogData :: AcidState StereoidDb -> CatalogId -> RouteT Sitemap App Response
catalogData sdb (CatalogId id) = undefined

startJob :: AcidState StereoidDb -> RouteT Sitemap App Response
startJob sdb = do
    id <- getFreeJobId sdb  
    liftIO $ forkIO $ addToStereoidDb id "/mnt/emusic" sdb
    ok $ toResponse "Job started"

jobsAll :: AcidState StereoidDb -> RouteT Sitemap App Response
jobsAll sdb = do
    jobs <- getJobs sdb
    ok $ toResponse $ toJSON jobs

jobData :: AcidState StereoidDb -> JobId -> RouteT Sitemap App Response
jobData sdb (JobId id) = do
    job <- getJob sdb id
    case job of
        Nothing -> notFound $ toResponse "What you are looking for has not been found."
        Just x  -> ok $ toResponse $ toJSON x

checkToken :: AcidState SessionMap -> Integer -> 
              RouteT Sitemap App Response -> 
              RouteT Sitemap App Response
checkToken sessions min f = do
    token <- msum [lookCookieValue "token", look "token"]
    user <- checkExtendSession sessions 15 token   
    case user of
        Nothing -> unauthorized $ toResponse $ "Invalid token"
        Just _  -> f

addUser :: AcidState UserMap -> RouteT Sitemap App Response
addUser acid = do
    user <- look "username"
    pass <- look "password"
    result <- (newUser acid user pass)  
    case result of
        Right _ -> ok (toResponse $ "User " ++ user ++ " created.")
        Left m  -> forbidden (toResponse $ show m)

authorize :: AcidState UserMap -> AcidState SessionMap -> RouteT Sitemap App Response
authorize users sessions = do
    username <- look "username"
    lt <- getDataFn $ look "logintoken"
    case lt of
        (Left _ ) -> do
            hash <- look "auth"
            salt <- look "timestamp"
            auth <- (authUser users username hash salt)
            if auth then do
                token <- (newSession sessions 60 username) 
                rq <- getDataFn $ look "rememberme"
                case rq of
                    (Left _) -> ok $ toResponse $ toJSON Session { sessionToken = token }
                    (Right _) -> do lT <- newRememberMe sessions username
                                    ok $ toResponse $ toJSON Remember { rSessionToken = token, rRememberToken = lT }
                else
                    forbidden $ toResponse $ "Invalid username/password"
        (Right logintoken) -> do
            rq <- checkRenewRememberMe sessions logintoken username
            case rq of
                Nothing -> forbidden $ toResponse $ "Invalid login token"
                Just newtoken -> do
                   sess <- newSession sessions 60 username
                   ok $ toResponse $ toJSON Remember { rSessionToken = sess, rRememberToken = newtoken }
            
songAddUrl :: I.Song -> RouteT Sitemap App Song
songAddUrl I.Song { I.songID  = id
                  , I.songName = name
                  , I.songTrack = track
                  , I.songAlbumId = albumID
                  , I.songAlbumTitle = albumtitle
                  , I.songArtistName = artistname
                  , I.songDuration = time
                  } = do surl <- (showURL (Stream (SongId id)  ))
                         aurl <- (showURL (AlbumArt (AlbumId albumID) ))
                         return Song { songID  = id
                                     , songName = name
                                     , songTrack = track
                                     , songUrl = surl
                                     , songArtUrl = aurl
                                     , songAlbumId = albumID
                                     , songAlbumTitle = albumtitle
                                     , songArtistName = artistname
                                     , songDuration = time
                                     } 

albumAddUrl :: I.Album -> RouteT Sitemap App Album
albumAddUrl I.Album { I.albumID = id
                    , I.albumTitle = title
                    , I.albumYear = year
                    , I.albumArtistID = artistid
                    , I.albumArtistName = artist
                    } = do aurl <- (showURL (AlbumArt (AlbumId id)))
                           turl <- (showURL (AlbumArtThumb (AlbumId id)))
                           surl <- (showURL (AlbumSongs (AlbumId id)))
                           murl <- (showURL (AlbumM3U (AlbumId id)))
                           return Album { albumID = id
                                        , albumTitle = title
                                        , albumArtistID = artistid
                                        , albumArtistName = artist
                                        , albumYear = year
                                        , albumArtUrl = aurl
                                        , albumArtThumbUrl = turl
                                        , albumSongsUrl = surl
                                        , albumM3UUrl = murl
                                        }

albumSongs :: AcidState StereoidDb -> AlbumId -> RouteT Sitemap App Response
albumSongs sdb (AlbumId id) = do
    sgs <- getSongsByAlbumId sdb id
    songs <- mapM songAddUrl sgs 
    ok $ toResponse $ toJSON $ sortBy (comparing songTrack) songs 

albumSongsM3U :: AcidState StereoidDb -> AlbumId -> RouteT Sitemap App Response
albumSongsM3U sdb (AlbumId id) = do
    sgs <- getSongsByAlbumId sdb id
    songs <- mapM songAddUrl sgs 
    case songs of
        [] -> notFound $ toResponse "What you are looking for has not been found."
        xs -> ok $ toResponse $ createM3u xs

albumData :: AcidState StereoidDb -> AlbumId -> RouteT Sitemap App Response
albumData sdb (AlbumId albumid) = do
    album <- getAlbum sdb albumid
    case album of
        Nothing -> notFound $ toResponse "What you are looking for has not been found."
        Just al -> do alb <- albumAddUrl al
                      ok $ toResponse $ toJSON alb

getArtFromUrl :: AcidState StereoidDb -> AlbumId -> RouteT Sitemap App Response
getArtFromUrl sdb (AlbumId artid) = do
    qs <- getDataFn $ lookRead "url"
    al <- getAlbum sdb artid
    case al of
        (Just album) -> do 
                            image <- case qs of
                                         (Left _)  -> do
                                                fmr <- liftIO $ getLastFmArtUrl (E.decodeUtf8 $ I.albumArtistName album) (E.decodeUtf8 $ I.albumTitle album)
                                                case fmr of
                                                    Nothing  -> return (Left "error")
                                                    Just url -> liftIO $ downloadFileWithMime url
                                         (Right u) -> liftIO $ downloadFileWithMime u
                            case image of
                                Left _                  -> notFound $ toResponse "What you are looking for has not been found."
                                Right (Just mime, resp) -> do liftIO $ BS.writeFile (afn artid) resp
                                                              addArt sdb artid mime (afn artid)
                                                              ok $ toResponse "Art added."
                                                              where afn x = ("art/" ++ (show x))
        _            -> notFound $ toResponse "What you are looking for has not been found."

{-
getThumbFromUrl :: AcidState StereoidDb -> AlbumId -> RouteT Sitemap App Response
getThumbFromUrl sdb (AlbumId artid) = do
    url <- look "url"
    image <- liftIO $ downloadFileWithMime url
    case image of
        Left x                  -> notFound $ toResponse "What you are looking for has not been found."
        Right (Just mime, resp) -> do liftIO $ BS.writeFile (afn artid) resp
                                      addThumb sdb artid mime (afn artid)
                                      ok $ toResponse "Thumb added."
                                      where afn x = ("thumb/" ++ (show x))
-}
serveArt :: AcidState StereoidDb -> String -> String -> AlbumId -> RouteT Sitemap App Response
serveArt sdb rd sd (AlbumId artid) = do
    dr <- getArt sdb artid
    case dr of
        Just (mime,art) -> serveFileUsing filePathSendAllowRange (asContentType $ B.toString mime) $ (sd ++ art)
        Nothing         -> serveFile (asContentType "image/png") (rd ++ "media_album.png")

serveGenThumb :: AcidState StereoidDb -> String -> String -> AlbumId -> RouteT Sitemap App Response
serveGenThumb sdb rd sd (AlbumId artid) = do
    dr <- getThumb sdb artid
    case dr of
        Just (tmime,tart) -> serveFileUsing filePathSendAllowRange (asContentType $ B.toString tmime) $ tart
        Nothing         -> do
            aa <- getArtData sdb artid
            case aa of
                Nothing         -> serveFile (asContentType "image/png") (rd ++ "media_album.png")
                Just (AlbumArtData amime file _  _) -> do
                    let tfn = ("thumb/" ++ (show artid))
                    result <- liftIO $ system $ "convert " ++ file ++ " -resize '200x200!>' " ++ (sd ++ tfn)
                    case result of
                        ExitSuccess   -> do
                            insertRowAlbumArtDb sdb artid (AlbumArtData amime file (Just amime) (Just tfn))
                            serveFileUsing filePathSendAllowRange (asContentType $ B.toString amime) $ (sd ++ tfn)
                        ExitFailure _ -> serveFile (asContentType "image/png") (rd ++ "media_album.png")
        
serveThumb :: AcidState StereoidDb -> AlbumId -> RouteT Sitemap App Response
serveThumb sdb (AlbumId artid) = do
    dr <- getThumb sdb artid
    case dr of
        Just (mime,art) -> serveFileUsing filePathSendAllowRange (asContentType $ B.toString mime) $ art
        Nothing         -> serveFile (asContentType "image/png") "media_album.png"

serveSong :: AcidState StereoidDb -> SongId -> RouteT Sitemap App Response
serveSong sdb SongId { unSongId = songid } = do
    song <- getSongFile sdb songid
    case song of
        Nothing -> notFound $ toResponse "What you are looking for has not been found."
        Just sf -> serveFileUsing filePathSendAllowRange (asContentType "audio/mpeg3") $ C.unpack sf

artistData :: AcidState StereoidDb -> ArtistId -> RouteT Sitemap App Response
artistData sdb (ArtistId id) = do
    artist <- getArtist sdb id
    case artist of
        Nothing -> notFound $ toResponse "What you are looking for has not been found."
        Just ar -> ok $ toResponse $ toJSON ar

artistAlbums :: AcidState StereoidDb -> ArtistId -> RouteT Sitemap App Response
artistAlbums sdb (ArtistId id) = do
    albs <- getAlbumsByArtistId sdb id
    albums <- mapM albumAddUrl albs 
    ok $ toResponse $ toJSON albums

artistsAll :: AcidState StereoidDb -> RouteT Sitemap App Response
artistsAll sdb = do
    artists <- getArtists sdb
    ok $ toResponse $ toJSON artists

songsAll :: AcidState StereoidDb -> RouteT Sitemap App Response
songsAll _ = do
    sdb <- getAcidState
    ol <- getOffsetLimit
    filter <- getDataFn $ lookRead "title"
    let fSongs = case filter of
                (Left e) -> getSongs sdb ol
                (Right r) -> filterSongTrie sdb (E.encodeUtf8 $ T.toUpper $ T.pack r)
    sos <- fSongs
    songs <- mapM songAddUrl sos
    ok $ toResponse $ toJSON songs

albumsAll :: AcidState StereoidDb -> RouteT Sitemap App Response
albumsAll sdb = do
    ol <- getOffsetLimit
    filter <- getDataFn $ lookRead "artist"
    sort <- getDataFn $ lookRead "sort"
    let getAlbs = case filter of
                (Left e) -> case sort of
                    (Left e) -> getAlbums sdb ol
                    (Right "random") -> do
                        s <- getDataFn $ lookRead "seed"
                        case s of
                            (Left e) -> getAlbums sdb ol
                            (Right r) -> getAlbumsRandom sdb ol r
                (Right r) -> filterArtistTrie sdb (E.encodeUtf8 $ T.toUpper $ T.pack r)
    albs <- getAlbs
    albums <- mapM albumAddUrl albs
    ok $ toResponse $ toJSON albums

getOffsetLimit :: RouteT Sitemap App (Int,Int)
getOffsetLimit  = do
    r <- getDataFn $ lookRead "offset"
    s <- getDataFn $ lookRead "limit"
    let offset = case r of
                    (Left e)  -> 0 
                    (Right i) -> i
    let limit = case s of
                    (Left e)  -> 50 
                    (Right i) -> i
    return (offset,limit)




