{-# LANGUAGE OverloadedStrings #-}
module Jobs where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Control.Monad.Trans (MonadIO, liftIO)
import System.FilePath (takeExtension)
import Data.Time.Clock.POSIX (getPOSIXTime)

import FileSystem
import Persistence

import Data.Acid
import Data.Acid.Advanced
import Types

import qualified Data.Text as T (toUpper,pack)
import qualified Data.ByteString.UTF8 as B (ByteString,fromString)
import qualified Data.ByteString.Char8 as C (pack)
import qualified Data.IntMap as IntMap (map)
import qualified Data.Text.Encoding as E

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

checkAddAlbumMap :: (Monad m, MonadIO m) => AcidState StereoidDb -> TagFileInfo -> m (Int,Maybe AlbumMapData)
checkAddAlbumMap sdb tags = do
    let aad = amd tags
    qr <- getAlbumMapId sdb aad    
    case qr of
        Just id -> return (id,Nothing)
        Nothing -> do new <- addToAlbumMap sdb tags
                      liftIO $ putStrLn $ "added album #" ++ (show new)
                      return (new, Just aad)
    where amd t = AlbumMapData { almdTitle = T.toUpper $ T.pack (tfiAlbum t)
                               , almdArtistName = T.toUpper $ T.pack (tfiArtist t)
                               , almdYear = tfiYear tags
                               }

checkAddArtistMap :: (Monad m, MonadIO m) => AcidState StereoidDb -> TagFileInfo -> m (Int,Maybe String)
checkAddArtistMap sdb tags = do
    qr <- getArtistMapId sdb (amd tags)    
    case qr of
        Just id -> return (id,Nothing)
        Nothing -> do new <- addToArtistMap sdb tags
                      liftIO $ putStrLn $ "added artist #" ++ (show new)
                      return (new,Just $ tfiArtist tags)
    where amd t = ArtistMapData { armdName = T.toUpper $ T.pack $ tfiArtist t }

addSongToStereoidDb :: (Monad m, MonadIO m) => AcidState StereoidDb -> TagFileInfo -> Int -> Int -> B.ByteString -> m Int
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
    return qr

processArt :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> Maybe String -> m ()
processArt _ _ (Nothing) = return ()
processArt sdb id (Just art) = do
    insertRowArtistDb sdb id (ArtistData (name art) (sortname art))
    insertRowArtistCache sdb id (ArtistCacheData (name art) (sortname art) [] )     
    insertKeyArtistTrie sdb (f art)
    where name = B.fromString
          sortname = B.fromString . fst . (splitPrefix prefixList)
          f = E.encodeUtf8 . (stripPrefix prefixList') . T.toUpper . T.pack

processAlb :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> Maybe AlbumMapData -> Int -> Maybe String -> m ()
processAlbum _ _ Nothing _ _ = return ()
processAlbum sdb id (Just alb) artid ad = do
    let artdata = case ad of
        (Just x)    -> x
        Nothing     -> do
            
        
addToStereoidDb :: Int -> FilePath -> AcidState StereoidDb -> IO ()
addToStereoidDb jobid fp sdb = do
    insertRowJobsDb sdb jobid (Add JobRunning 0) 
    rd <- getRecursiveContents fp
    forM_ (filter takeMp3 rd) (doTag sdb jobid)
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
    updateJobStatus sdb jobid JobFinished
    where takeMp3 x = (takeExtension x) == ".mp3"
          doTag s j x = do
                fc <- getFileCacheData s (C.pack x) 
                case fc of
                    (Just fcd) -> return ()
                    Nothing    -> examineFile x j
                 where examineFile x j = do
                       tf <- getTagFileInfo x
                       case tf of
                            Nothing -> return ()
                            Just tags -> do
                               (art,artres) <- checkAddArtistMap s tags
                               processArt s art artres
                               (alb,albres) <- checkAddAlbumMap  s tags 
                               processAlb s alb albres art artres
                               sid          <- addSongToStereoidDb s tags art alb (C.pack x)
                               updateJobCount s j 1
                               putStrLn "+"
    
{-
main :: IO ()
main = bracket
    (openLocalState (sdbEmpty))
    (closeAcidState)
    (addToStereoidDb "/mnt/emusic")
-}
