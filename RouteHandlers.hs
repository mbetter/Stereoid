module RouteHandlers where

import DatabaseFunctions
import JsonInstances
import Routing
import DataStructures
import Persistence
import qualified DataStructuresInternal as I
import Data.Ord (comparing)
import Data.List (sortBy)
import Control.Monad (msum, liftM)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString as BS
import Database.HDBC
import Auth
import Data.Acid
import Data.Acid.Advanced
import Text.JSON
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
                    (Artists)               -> chk $ artistsAll sdb 
                    (Albums)                -> chk $ albumsAll sdb 
                    (AlbumSongs albumId)    -> chk $ albumSongs sdb albumId
                    (AlbumM3U albumId)      -> chk $ albumSongsM3U sdb albumId
                    (AlbumArt albumId)      -> chk $ serveArt sdb albumId
                    (AlbumArtThumb albumId) -> chk $ serveThumb sdb albumId
             PUT -> case url of
                    (Users)                 -> addUser users
                    (Sessions)              -> authorize users sessions


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
    hash <- look "auth"
    salt <- look "timestamp"
    auth <- (authUser users username hash salt)
    if auth then do
        token <- (newSession sessions 60 username) 
        let session = Session { sessionToken = token }
        Cookie.addCookie Cookie.Session (Cookie.mkCookie "token" token)
        ok $ toResponse $ showJSON session 
    else
        forbidden $ toResponse $ "Invalid username/password"

songAddUrl :: I.Song -> RouteT Sitemap (ServerPartT IO) Song
songAddUrl I.Song { I.songID  = id
                  , I.songName = name
                  , I.songTrack = track
                  , I.songAlbumId = albumID
                  , I.songAlbumTitle = albumtitle
                  , I.songArtistName = artistname
                  , I.songDuration = time
                  } = do surl <- (showURL (Stream (SongId id)  ))
                         return Song { songID  = id
                                     , songName = name
                                     , songTrack = track
                                     , songUrl = surl
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
    ok $ toResponse $ showJSON $ sortBy (comparing songTrack) songs 

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
                      ok $ toResponse $ showJSON alb

serveArt :: AcidState StereoidDb -> AlbumId -> RouteT Sitemap (ServerPartT IO) Response
serveArt sdb (AlbumId artid) = do
    dr <- getArt sdb artid
    case dr of
        Just (mime,art) -> serveFileUsing filePathSendAllowRange (asContentType $ B.toString mime) $ art
        Nothing         -> serveFile (asContentType "image/png") "media_album.png"

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
        Just ar -> ok $ toResponse $ showJSON ar

artistAlbums :: AcidState StereoidDb -> ArtistId -> RouteT Sitemap (ServerPartT IO) Response
artistAlbums sdb (ArtistId id) = do
    albs <- getAlbumsByArtistId sdb id
    albums <- mapM albumAddUrl albs 
    ok $ toResponse $ showJSON albums

artistsAll :: AcidState StereoidDb -> RouteT Sitemap (ServerPartT IO) Response
artistsAll sdb = do
    artists <- getArtists sdb
    ok $ toResponse $ showJSON artists

albumsAll :: AcidState StereoidDb -> RouteT Sitemap (ServerPartT IO) Response
albumsAll sdb = do
    ol <- getOffsetLimit
    albs <- getAlbums sdb ol
    albums <- mapM albumAddUrl albs
    ok $ toResponse $ showJSON albums

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


