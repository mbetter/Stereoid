{-# LANGUAGE OverloadedStrings #-}
module Conversion where

import Stereoid.Persistence
import Stereoid.Types
import Database.HDBC
import Database.HDBC.MySQL
import qualified Data.ByteString as BS
import Control.Monad           (msum, mzero, MonadPlus)
import Control.Monad.Trans     (MonadIO, liftIO)
import Data.Acid
import Data.Acid.Advanced
import qualified Data.IntMap as IntMap
import qualified Data.ByteString.UTF8 as B

getFilesFromDb :: (MonadIO m, IConnection d, MonadPlus m) => d -> m [(BS.ByteString, FileCacheData)]
getFilesFromDb db = do
    songs <- liftIO $ handleSqlError $ quickQuery' db "SELECT id,file,update_time,addition_time FROM `song`" []  
    return $ map fileFromDbRow songs
               where fileFromDbRow [id, file,update,addition] = ((fromSql file), item)
                       where item = FileCacheData { fcdSongId = fromSql id
                                                  , fcdAddTime = fromSql addition
                                                  , fcdUpdateTime  = fromSql update
                                                  }

getSongsFromDb :: (MonadIO m, IConnection d, MonadPlus m) => d -> m [(Int, SongData)]
getSongsFromDb db = do
    songs <- liftIO $ handleSqlError $ quickQuery' db "SELECT id,title,track,file,album,artist,time,year FROM `song`" []  
    return $ map songFromDbRow songs
               where songFromDbRow [id,title,track,file,album,artist,time,year] = ((fromSql id), item)
                       where item = SongData { sodName = fromSql title
                                             , sodTrack = fromSql track
                                             , sodFile = fromSql file
                                             , sodAlbumId = fromSql album
                                             , sodArtistId = fromSql artist
                                             , sodYear = fromSql year
                                             , sodDuration = fromSql time
                                             }

getAlbumsFromDb :: (MonadIO m, IConnection d, MonadPlus m) => d -> m [(Int, AlbumData)]
getAlbumsFromDb db = do
    albums <- liftIO $ handleSqlError $ quickQuery' db "SELECT id,name,prefix FROM `album`" []  
    return $ map albumFromDbRow albums
               where albumFromDbRow [alid,albumname,albumpre] = ((fromSql alid), item)
                       where item = AlbumData { aldTitle = (formatWithPrefix (fromSql albumname) (fromSql albumpre)), aldSortTitle = (fromSql albumname) }

getArtistsFromDb :: (MonadIO m, IConnection d, MonadPlus m) => d -> m [(Int, ArtistData)]
getArtistsFromDb db = do
    artists <- liftIO $ handleSqlError $ quickQuery' db "SELECT id,name,prefix FROM `artist`" []  
    return $ map artistFromDbRow artists
               where artistFromDbRow [alid,artistname,artistpre] = ((fromSql alid), item)
                       where item = ArtistData { ardName = (formatWithPrefix (fromSql artistname) (fromSql artistpre)), ardSortName = (fromSql artistname) }

formatWithPrefix :: B.ByteString -> Maybe B.ByteString -> B.ByteString
formatWithPrefix str Nothing = str
formatWithPrefix str (Just prefix) = B.fromString $ (B.toString prefix) ++ " " ++ (B.toString str)

getAllArtFromDb :: (IConnection d, MonadIO m, MonadPlus m) => d -> AcidState StereoidDb -> m ()
getAllArtFromDb db sdb = do
    arts <- liftIO $ handleSqlError $ quickQuery' db "SELECT `album_id`,`art_mime`,`art`,`thumb_mime`,`thumb` FROM `album_data`" []
    liftIO $ mapM_ (artFromDbRow) arts
             where artFromDbRow [id,mime,art,tmime,tart] = do 
                       let (cid, cmime, cart, ctmime, ctart) = (fromSql id, fromSql mime, fromSql art, fromSql tmime, fromSql tart)
                       insertRowAlbumArtDb sdb cid $ AlbumArtData { aadMime = cmime, aadArtFile = (afn (show cid)), aadThumbMime = ctmime, aadThumbFile = (tfn ctmime (show cid))}
                       BS.writeFile (afn (show cid)) cart 
                       case (tfn ctmime (show cid)) of
                            (Just fn) -> BS.writeFile fn ctart
                            Nothing -> return ()
                       where afn x = ("art/" ++ x)
                             tfn (Just _) x= Just ("thumb/" ++ x)
                             tfn Nothing _= Nothing

main :: IO ()
main = do
        sdb <- openLocalState sdbEmpty
        
        -- ALBUMS DONE, TODO: SONGS AND ARTISTS
        db <- connectMySQL defaultMySQLConnectInfo {
                mysqlHost       = "db.lan",
                mysqlUser       = "ampache",
                mysqlPassword   = "FOY@Q@B",
                mysqlDatabase   = "ampache"
              }
        
        
        putStrLn "Reading albums from db..."
        albumdata <- getAlbumsFromDb db
        putStrLn "Inserting in acid..."
        mapM (uncurry (insertRowAlbumDb sdb)) albumdata 

        putStrLn "Reading artists from db..."
        artistdata <- getArtistsFromDb db
        putStrLn "Inserting in acid..."
        mapM (uncurry (insertRowArtistDb sdb)) artistdata 

        putStrLn "Reading songs from db..."
        songdata <- getSongsFromDb db
        putStrLn "Inserting in acid..."
        mapM (uncurry (insertRowSongDb sdb)) songdata 
        
        putStrLn "Getting all art..."
        getAllArtFromDb db sdb

        putStrLn "Building album cache..."
        buildAlbumCache sdb

        putStrLn "Building artist cache..."
        buildArtistCache sdb

        putStrLn "Building album map..."
        buildAlbumMap sdb

        putStrLn "Building artist map..."
        buildArtistMap sdb

        putStrLn "Reading files from db..."
        filedata <- getFilesFromDb db
        putStrLn "Inserting in acid..."
        mapM (uncurry (insertRowFileCache sdb)) filedata 
        
        putStrLn "Building artist trie..."
        buildArtistTrie sdb

        putStrLn "Building song cache ..."
        buildSongCache sdb

        putStrLn "Building song trie ..."
        buildSongTrie sdb

        putStrLn "Building stats..."
        buildStats sdb
        putStrLn "Done!"

        disconnect db 
        closeAcidState sdb
        putStrLn "Done."

