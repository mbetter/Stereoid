module DatabaseFunctions where

import Database.HDBC
import qualified Data.ByteString as B
import Control.Monad           (msum, mzero, MonadPlus)
import Control.Monad.Trans     (MonadIO, liftIO)
import Web.Routes              ( PathInfo(..), RouteT    , showURL , runRouteT
                               , Site(..)    , setDefault, mkSitePI           )
import Happstack.Server        (port      , Response     , ServerPartT, ok           , toResponse
                               ,simpleHTTP, nullConf     , seeOther   , dir          , notFound
                               ,seeOther  , asContentType, serveFile  , ToMessage(..)                                                    )
import Routing
import DataStructures

getThumbFromDb :: (IConnection d, MonadIO m, MonadPlus m) => d -> Int -> m (Maybe (String, B.ByteString))
getThumbFromDb db id = do
    arts <- liftIO $ handleSqlError $
                quickQuery' db "SELECT `thumb_mime`,`thumb` FROM `album_data` WHERE `album_id`= ?" [toSql id]
    case arts of
        ([SqlNull,_]:_) -> return Nothing
        ([_,SqlNull]:_) -> return Nothing
        ([mime,bs]:_) ->
            return $ Just ((fromSql mime),(fromSql bs))
        _          -> mzero

getArtFromDb :: (IConnection d, MonadIO m, MonadPlus m) => d -> Int -> m (Maybe (String, B.ByteString))
getArtFromDb db id = do
    arts <- liftIO $ handleSqlError $
                quickQuery' db "SELECT `art_mime`,`art` FROM `album_data` WHERE `album_id`= ?" [toSql id]
    case arts of
        ([SqlNull,_]:_) -> return Nothing
        ([_,SqlNull]:_) -> return Nothing
        ([mime,bs]:_) ->
            return $ Just ((fromSql mime),(fromSql bs))
        _          -> mzero

{-
albumQuery = "SELECT DISTINCT song.album,album.name,album.prefix,song.artist,artist.name,artist.prefix \
             \FROM `song` \
             \LEFT JOIN `album` \
             \ON song.album=album.id \
             \LEFT JOIN `artist` \
             \ON song.artist=artist.id" 

artistQuery = "SELECT artist.id,artist.name,artist.prefix FROM `artist`"

queryFilters :: String -> [(String,SqlValue)] -> (String,[SqlValue])
queryFilters sql [] = (sql, [])
queryFilters sql al = ((sql ++ (doWhere x) ++ ( concat $ map doFilter xs)), params) 
                       where ((x:xs),params) = unzip al
                             doFilter []  = []
                             doFilter str = " AND " ++ str ++ "=?"
                             doWhere []  = []
                             doWhere str = " WHERE " ++ str ++ "=?"

getAlbumsFromDb :: (IConnection d) => d -> [(String,SqlValue)] -> RouteT Sitemap (ServerPartT IO) [Album]
getAlbumsFromDb db params = do
    albums <- liftIO $ handleSqlError $ uncurry (quickQuery' db) (queryFilters albumQuery params)  
    case albums of
        (x:xs) -> mapM albumFromDbRow albums
                  where albumFromDbRow [alid,albumname,albumpre,artid,artist,artistpre] = do
                            aurl <- (showURL (AlbumArt (AlbumId (fromSql alid))))
                            return Album { albumID         = (fromSql alid)
                                         , albumTitle      = (formatWithPrefix (fromSql albumname) (fromSql albumpre))
                                         , albumArtistID   = (fromSql artid)
                                         , albumArtistName = (formatWithPrefix (fromSql artist) (fromSql artistpre))
                                         , albumArtUrl     = aurl
                                         }
        _       -> mzero

getArtistsFromDb :: (IConnection d, MonadIO m, MonadPlus m) => d -> [(String,SqlValue)] -> m [Artist]
getArtistsFromDb db params = do
    artists <- liftIO $ handleSqlError $ uncurry (quickQuery' db) (queryFilters artistQuery params) 
    case artists of
        (x:xs) ->              return $ map artistFromDbRow artists
                               where artistFromDbRow [id,artnam,artpre] = 
                                         Artist { artistID         = (fromSql id)
                                                , artistName = (formatWithPrefix (fromSql artnam) (fromSql artpre))
                                                }
        _                                 -> mzero

getArtistFromDb :: (IConnection d, MonadIO m, MonadPlus m) => d -> ArtistId -> m Artist
getArtistFromDb db (ArtistId id) = do
    artists <- liftIO $ handleSqlError $
               quickQuery' db "SELECT artist.name,artist.prefix \
                              \FROM `artist` \
                              \WHERE artist.id = ?" [toSql id]
    case artists of
        ([artnam,artpre]:_) -> do
              return Artist { artistID         = id
                           , artistName = (formatWithPrefix (fromSql artnam) (fromSql artpre))
                           }
        _                                 -> mzero

getAlbumFromDb :: (IConnection d) => d -> AlbumId -> RouteT Sitemap (ServerPartT IO) Album
getAlbumFromDb db albumid@(AlbumId id) = do
    albums <- liftIO $ handleSqlError $
              quickQuery' db "SELECT album.name,album.prefix,song.artist,artist.name,artist.prefix \
                             \FROM `song` \
                             \LEFT JOIN `album` \
                             \ON song.album=album.id \
                             \LEFT JOIN `artist` \
                             \ON song.artist=artist.id \
                             \WHERE song.album = ?" [toSql id]
    case albums of
        ([nam,pre,artid,artnam,artpre]:_) -> do
              aurl <- (showURL (AlbumArt albumid))
              return Album { albumID         = id
                           , albumTitle      = (formatWithPrefix (fromSql nam) (fromSql pre))
                           , albumArtistID   = (fromSql artid)
                           , albumArtistName = (formatWithPrefix (fromSql artnam) (fromSql artpre))
                           , albumArtUrl     = aurl
                           }
        _                                 -> mzero

getAlbumSongsFromDb :: (IConnection d) => d -> Int -> RouteT Sitemap (ServerPartT IO) [Song]
getAlbumSongsFromDb db id = do
    songs <- liftIO $ handleSqlError $
                quickQuery' db "SELECT song.id,song.title,song.track,song.album,album.name,album.prefix,artist.name,artist.prefix,song.time \
                               \FROM `song` \
                               \LEFT JOIN `album` \
                               \ON song.album=album.id \
                               \LEFT JOIN `artist` \
                               \ON song.artist=artist.id \
                               \WHERE song.album = ?" [toSql id]
    case songs of
        (x:xs) -> mapM songFromDbRow songs
                  where songFromDbRow [sid,title,track,albumid,albumname,albumpre,artist,artistpre,dur] = do
                        surl <- (showURL (Stream (SongId (fromSql sid)  )))
                        return Song { songID         = (fromSql sid)
                                    , songName       = (fromSql title)
                                    , songTrack      = (fromSql track)
                                    , songUrl        = surl
                                    , songAlbumId    = (fromSql albumid)
                                    , songAlbumTitle = (formatWithPrefix (fromSql albumname) (fromSql albumpre))
                                    , songArtistName = (formatWithPrefix (fromSql artist) (fromSql artistpre))
                                    , songDuration   = (fromSql dur)
                                    } 
        _       -> mzero

getSongUrlFromDb :: (IConnection d, MonadIO m, MonadPlus m) => d -> Int -> m String
getSongUrlFromDb db id = do
    songs <- liftIO $ handleSqlError $
                quickQuery' db "SELECT `file` FROM `song` WHERE id = ?" [toSql id]
    case songs of
        ([file]:_) ->
            return (fromSql file)
        _          -> mzero
-}



