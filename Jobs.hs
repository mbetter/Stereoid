{-# LANGUAGE OverloadedStrings #-}
module Jobs where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Control.Monad.Trans (MonadIO, liftIO)
import System.FilePath (takeExtension)
import Data.Time.Clock.POSIX (getPOSIXTime)

import FileSystem
import Persistence
import Persistence.Types

import Data.Acid
import Data.Acid.Advanced

import qualified Data.Text as T (toUpper,pack)
import qualified Data.ByteString.UTF8 as B (ByteString,fromString)
import qualified Data.ByteString.Char8 as C (pack)
import qualified Data.IntMap as IntMap (map)

import Audio.TagLib.TagLib

clearThumbnails :: AcidState StereoidDb -> IO ()
clearThumbnails acid = do
    art <- query acid (QueryAlbumArt)
    update acid (InsertAlbumArtDb $ AlbumArtDb $ IntMap.map clearthumbnails art)
    where clearthumbnails (AlbumArtData m a _ _) = AlbumArtData m a Nothing Nothing 

cleanStereoidDb :: AcidState StereoidDb -> IO ()
cleanStereoidDb sdb = undefined

updateStereoidDb :: AcidState StereoidDb -> IO()
updateStereoidDb sdb = undefined

data TagFileInfo = TagFileInfo { tfiTitle :: String
                               , tfiTrack :: Int
                               , tfiArtist :: String
                               , tfiAlbum :: String
                               , tfiYear :: Int
                               , tfiDuration :: Int
                               }

getTagFileInfo :: FilePath -> IO (Maybe TagFileInfo)
getTagFileInfo s = do
    t <- tagFileOpen $ C.pack s
    case t of
        Nothing -> return Nothing
        Just r  -> do
            tt <- tagFileGetTag r
            aa <- tagFileGetAudioProperties r
            case (tt,aa) of
                (Nothing,_)  -> return Nothing
                (_,Nothing)  -> return Nothing
                (Just tag,Just ap)  -> do
                        tit <- tagGetTitle tag
                        tra <- tagGetTrack tag
                        art <- tagGetArtist tag
                        alb <- tagGetAlbum tag
                        yea <- tagGetYear tag
                        dur <- audioPropertiesGetDuration ap
                        return $ Just TagFileInfo { tfiTitle = tit
                                                  , tfiTrack = tra
                                                  , tfiArtist = art
                                                  , tfiAlbum = alb
                                                  , tfiYear = yea
                                                  , tfiDuration = dur
                                                  }
                                                  
addToAlbumMap :: (Monad m, MonadIO m) => AcidState StereoidDb -> TagFileInfo -> m Int 
addToAlbumMap sdb tags = do
    albid <- getFreeAlbumId sdb
    insertRowAlbumMap sdb (amd tags) albid
    return albid
    where amd t = AlbumMapData { almdTitle = T.toUpper $ T.pack (tfiAlbum t)
                               , almdArtistName = T.toUpper $ T.pack (tfiArtist t)
                               , almdYear = tfiYear tags
                               }
   
addToArtistMap :: (Monad m, MonadIO m) => AcidState StereoidDb -> TagFileInfo -> m Int 
addToArtistMap sdb tags = do
    artid <- getFreeArtistId sdb
    insertRowArtistMap sdb (amd tags) artid
    return artid
    where amd t = ArtistMapData { armdName = T.toUpper $ T.pack $ tfiArtist t }

checkAddAlbumMap :: (Monad m, MonadIO m) => AcidState StereoidDb -> TagFileInfo -> m Int
checkAddAlbumMap sdb tags = do
    qr <- getAlbumMapId sdb (amd tags)    
    case qr of
        Just id -> return id
        Nothing -> do new <- addToAlbumMap sdb tags
                      liftIO $ putStrLn $ "added album #" ++ (show new)
                      return new
    where amd t = AlbumMapData { almdTitle = T.toUpper $ T.pack (tfiAlbum t)
                               , almdArtistName = T.toUpper $ T.pack (tfiArtist t)
                               , almdYear = tfiYear tags
                               }

checkAddArtistMap :: (Monad m, MonadIO m) => AcidState StereoidDb -> TagFileInfo -> m Int
checkAddArtistMap sdb tags = do
    qr <- getArtistMapId sdb (amd tags)    
    case qr of
        Just id -> return id
        Nothing -> do new <- addToArtistMap sdb tags
                      liftIO $ putStrLn $ "added artist #" ++ (show new)
                      return new
    where amd t = ArtistMapData { armdName = T.toUpper $ T.pack $ tfiArtist t }

addSongToStereoidDb :: (Monad m, MonadIO m) => AcidState StereoidDb -> TagFileInfo -> Int -> Int -> B.ByteString -> m ()
addSongToStereoidDb sdb t artist album file = do
    qr <- getFreeSongId sdb
    now <- liftIO $ getPOSIXTime
    insertRowSongDb sdb qr SongData { sodName = B.fromString $ tfiTitle t
                                    , sodTrack = tfiTrack t
                                    , sodYear = tfiYear t
                                    , sodFile = file
                                    , sodAlbumId = album
                                    , sodArtistId = artist
                                    , sodDuration = tfiDuration t
                                    }
    insertRowAlbumDb sdb album AlbumData { aldTitle = B.fromString $ tfiAlbum t
                                         , aldSortTitle = B.fromString $ fst $ splitPrefix prefixList $ tfiAlbum t
                                         }
    insertRowArtistDb sdb artist ArtistData { ardName = B.fromString $ tfiArtist t
                                            , ardSortName = B.fromString $ fst $ splitPrefix prefixList $ tfiArtist t
                                            }
    insertRowFileCache sdb file FileCacheData { fcdSongId = qr
                                              , fcdAddTime = floor now
                                              , fcdUpdateTime = floor now
                                              }

addToStereoidDb :: Int -> FilePath -> AcidState StereoidDb -> IO ()
addToStereoidDb jobid fp sdb = do
    rd <- getRecursiveContents fp
    forM_ (filter takeMp3 rd) (doTag sdb)
    putStrLn "rebuilding album cache..."
    buildAlbumCache sdb
    putStrLn "rebuilding artist cache..."
    buildArtistCache sdb
    putStrLn "Building album map..."
    buildAlbumMap sdb
    putStrLn "Building artist map..."
    buildArtistMap sdb
    putStrLn "Building artist trie..."
    buildArtistTrie sdb
    putStrLn "Building song cache ..."
    buildSongCache sdb
    putStrLn "Building song trie ..."
    buildSongTrie sdb
    putStrLn "Building stats..."
    buildStats sdb
    putStrLn "Done!"

    where takeMp3 x = (takeExtension x) == ".mp3"
          doTag s x = do
                fc <- getFileCacheData s (C.pack x) 
                case fc of
                    (Just fcd) -> return ()
                    Nothing    -> examineFile x
                 where examineFile x = do
                       tf <- getTagFileInfo x
                       case tf of
                            Nothing -> return ()
                            Just tags -> do
                               art <- checkAddArtistMap s tags
                               alb <- checkAddAlbumMap  s tags 
                               addSongToStereoidDb s tags art alb (C.pack x)
                               putStrLn "+"
    
{-
main :: IO ()
main = bracket
    (openLocalState (sdbEmpty))
    (closeAcidState)
    (addToStereoidDb "/mnt/emusic")
-}
