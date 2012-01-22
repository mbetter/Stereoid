module RouteHandlers where

import JsonInstances
import Routing
import DataStructures
import Persistence
import Persistence.Types
import qualified DataStructuresInternal as I
import Data.Ord (comparing)
import Data.List (sortBy)
import Control.Monad (msum, liftM)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import HTTPClient
import Jobs
import Auth
import Data.Acid
import Data.Acid.Advanced
import Data.Aeson
import LastFM.Request
import System.Process
import System.Exit  ( ExitCode(..) )
import Control.Applicative (optional)
import Control.Monad.Trans     (MonadIO, liftIO)
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
import Control.Concurrent (killThread, forkIO)
import Web.Routes.Boomerang    
import Web.Routes              ( PathInfo(..), RouteT    , showURL , runRouteT
                               , Site(..)    , setDefault, mkSitePI           )

decodePolicy :: BodyPolicy
decodePolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

site :: AcidState StereoidDb -> AcidState UserMap -> AcidState SessionMap -> Site Sitemap (ServerPartT IO Response)
site sdb users sessions = setDefault Home $ boomerangSite (runRouteT (route sdb users sessions)) sitemap

route :: AcidState StereoidDb -> AcidState UserMap -> AcidState SessionMap -> Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route sdb users sessions url =
     do decodeBody decodePolicy
        rq <- askRq
        let chk = checkToken sessions 15
        let meth = rqMethod rq
        case meth of
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
                    (AlbumArt albumId)      -> chk $ serveArt sdb albumId
                    (AlbumArtThumb albumId) -> chk $ serveGenThumb sdb albumId
                    (Jobs)                  -> chk $ jobsAll sdb
                    (JobInfo jobId)         -> chk $ jobData sdb jobId
             PUT -> case url of
                    (Sessions)              -> chk $ ok (toResponse "Session extended.")
                    (Users)                 -> addUser users
             POST -> case url of
                    (Sessions)              -> authorize users sessions
                    (AlbumArt albumId)      -> chk $ getArtFromUrl sdb albumId
                    (Jobs)                  -> chk $ startJob sdb


startJob :: AcidState StereoidDb -> RouteT Sitemap (ServerPartT IO) Response
startJob sdb = do
    id <- getFreeJobId sdb  
    liftIO $ forkIO $ addToStereoidDb id "/mnt/emusic" sdb
    ok $ toResponse "Job started"

jobsAll :: AcidState StereoidDb -> RouteT Sitemap (ServerPartT IO) Response
jobsAll sdb = do
    jobs <- getJobs sdb
    ok $ toResponse $ encode jobs

jobData :: AcidState StereoidDb -> JobId -> RouteT Sitemap (ServerPartT IO) Response
jobData sdb (JobId id) = do
    job <- getJob sdb id
    case job of
        Nothing -> notFound $ toResponse "What you are looking for has not been found."
        Just x  -> ok $ toResponse $ encode x

checkToken :: AcidState SessionMap -> Integer -> 
              RouteT Sitemap (ServerPartT IO) Response -> 
              RouteT Sitemap (ServerPartT IO) Response
checkToken sessions min f = do
    token <- msum [lookCookieValue "token", look "token"]
    user <- checkExtendSession sessions 15 token   
    case user of
        Nothing -> unauthorized $ toResponse $ "Invalid token"
        Just _  -> f

addUser :: AcidState UserMap -> RouteT Sitemap (ServerPartT IO) Response
addUser acid = do
    user <- look "username"
    pass <- look "password"
    result <- (newUser acid user pass)  
    case result of
        Right _ -> ok (toResponse $ "User " ++ user ++ " created.")
        Left m  -> forbidden (toResponse $ show m)

authorize :: AcidState UserMap -> AcidState SessionMap -> RouteT Sitemap (ServerPartT IO) Response
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
                    (Left _) -> ok $ toResponse $ encode Session { sessionToken = token }
                    (Right _) -> do lT <- newRememberMe sessions username
                                    ok $ toResponse $ encode Remember { rSessionToken = token, rRememberToken = lT }
            else
                forbidden $ toResponse $ "Invalid username/password"
        (Right logintoken) -> do
            rq <- checkRenewRememberMe sessions logintoken username
            case rq of
                Nothing -> forbidden $ toResponse $ "Invalid login token"
                Just newtoken -> do
                   sess <- newSession sessions 60 username
                   ok $ toResponse $ encode Remember { rSessionToken = sess, rRememberToken = newtoken }
            
songAddUrl :: I.Song -> RouteT Sitemap (ServerPartT IO) Song
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

albumAddUrl :: I.Album -> RouteT Sitemap (ServerPartT IO) Album
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

albumSongs :: AcidState StereoidDb -> AlbumId -> RouteT Sitemap (ServerPartT IO) Response
albumSongs sdb (AlbumId id) = do
    sgs <- getSongsByAlbumId sdb id
    songs <- mapM songAddUrl sgs 
    ok $ toResponse $ encode $ sortBy (comparing songTrack) songs 

albumSongsM3U :: AcidState StereoidDb -> AlbumId -> RouteT Sitemap (ServerPartT IO) Response
albumSongsM3U sdb (AlbumId id) = do
    sgs <- getSongsByAlbumId sdb id
    songs <- mapM songAddUrl sgs 
    case songs of
        [] -> notFound $ toResponse "What you are looking for has not been found."
        xs -> ok $ toResponse $ createM3u xs

albumData :: AcidState StereoidDb -> AlbumId -> RouteT Sitemap (ServerPartT IO) Response
albumData sdb (AlbumId albumid) = do
    album <- getAlbum sdb albumid
    case album of
        Nothing -> notFound $ toResponse "What you are looking for has not been found."
        Just al -> do alb <- albumAddUrl al
                      ok $ toResponse $ encode alb

getArtFromUrl :: AcidState StereoidDb -> AlbumId -> RouteT Sitemap (ServerPartT IO) Response
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
getThumbFromUrl :: AcidState StereoidDb -> AlbumId -> RouteT Sitemap (ServerPartT IO) Response
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
serveArt :: AcidState StereoidDb -> AlbumId -> RouteT Sitemap (ServerPartT IO) Response
serveArt sdb (AlbumId artid) = do
    dr <- getArt sdb artid
    case dr of
        Just (mime,art) -> serveFileUsing filePathSendAllowRange (asContentType $ B.toString mime) $ art
        Nothing         -> serveFile (asContentType "image/png") "media_album.png"

serveGenThumb :: AcidState StereoidDb -> AlbumId -> RouteT Sitemap (ServerPartT IO) Response
serveGenThumb sdb (AlbumId artid) = do
    dr <- getThumb sdb artid
    case dr of
        Just (tmime,tart) -> serveFileUsing filePathSendAllowRange (asContentType $ B.toString tmime) $ tart
        Nothing         -> do
            aa <- getArtData sdb artid
            case aa of
                Nothing         -> serveFile (asContentType "image/png") "media_album.png"
                Just (AlbumArtData amime file _  _) -> do
                    let tfn = ("thumb/" ++ (show artid))
                    result <- liftIO $ system $ "convert " ++ file ++ " -resize '200x200!>' " ++ tfn
                    case result of
                        ExitSuccess   -> do
                            insertRowAlbumArtDb sdb artid (AlbumArtData amime file (Just amime) (Just tfn))
                            serveFileUsing filePathSendAllowRange (asContentType $ B.toString amime) $ tfn
                        ExitFailure _ -> serveFile (asContentType "image/png") "media_album.png"
        
serveThumb :: AcidState StereoidDb -> AlbumId -> RouteT Sitemap (ServerPartT IO) Response
serveThumb sdb (AlbumId artid) = do
    dr <- getThumb sdb artid
    case dr of
        Just (mime,art) -> serveFileUsing filePathSendAllowRange (asContentType $ B.toString mime) $ art
        Nothing         -> serveFile (asContentType "image/png") "media_album.png"

serveSong :: AcidState StereoidDb -> SongId -> RouteT Sitemap (ServerPartT IO) Response
serveSong sdb SongId { unSongId = songid } = do
    song <- getSongFile sdb songid
    case song of
        Nothing -> notFound $ toResponse "What you are looking for has not been found."
        Just sf -> serveFileUsing filePathSendAllowRange (asContentType "audio/mpeg3") $ C.unpack sf

artistData :: AcidState StereoidDb -> ArtistId -> RouteT Sitemap (ServerPartT IO) Response
artistData sdb (ArtistId id) = do
    artist <- getArtist sdb id
    case artist of
        Nothing -> notFound $ toResponse "What you are looking for has not been found."
        Just ar -> ok $ toResponse $ encode ar

artistAlbums :: AcidState StereoidDb -> ArtistId -> RouteT Sitemap (ServerPartT IO) Response
artistAlbums sdb (ArtistId id) = do
    albs <- getAlbumsByArtistId sdb id
    albums <- mapM albumAddUrl albs 
    ok $ toResponse $ encode albums

artistsAll :: AcidState StereoidDb -> RouteT Sitemap (ServerPartT IO) Response
artistsAll sdb = do
    artists <- getArtists sdb
    ok $ toResponse $ encode artists

songsAll :: AcidState StereoidDb -> RouteT Sitemap (ServerPartT IO) Response
songsAll sdb = do
    ol <- getOffsetLimit
    filter <- getDataFn $ lookRead "title"
    let fSongs = case filter of
                (Left e) -> getSongs sdb ol
                (Right r) -> filterSongTrie sdb (E.encodeUtf8 $ T.toUpper $ T.pack r)
    sos <- fSongs
    songs <- mapM songAddUrl sos
    ok $ toResponse $ encode songs

albumsAll :: AcidState StereoidDb -> RouteT Sitemap (ServerPartT IO) Response
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
    ok $ toResponse $ encode albums

getOffsetLimit :: RouteT Sitemap (ServerPartT IO) (Int,Int)
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




