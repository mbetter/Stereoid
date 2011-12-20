{-# LANGUAGE OverloadedStrings #-}
module Id3Test where

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified ID3
import qualified ID3.Simple as I
import qualified Data.ByteString.UTF8 as B
import Control.Monad (guard)
import Control.Exception
import Control.Monad.Trans     (MonadIO, liftIO)
import System.Path
import System.FilePath
import System.IO.HVFS
import System.Directory (doesFileExist)
import System.IO.Error (isDoesNotExistError)
import Control.Monad (forM_)
import Persistence
import qualified Audio.TagLib.TagLib as TagLib
import Data.Accessor
import Data.Maybe
import ID3.Type
import ID3.ReadTag
import ID3.WriteTag
import Data.Acid
import Data.Acid.Advanced
import System.Posix.Files

type Tag = ID3Tag

getFrameText :: FrameID -> Tag -> Maybe String
getFrameText id tag = case tag^.frame id of
                           Nothing -> Nothing
                           Just fr -> Just (fr^.textContent)

                                             {-
                                             r <- tryJust (guard . isDoesNotExistError) $ getFileStatus x
                                             case r of
                                                Left e   -> return ()
                                                Right fs -> case ((modtime fs) > ((dbmod fcd)-500)) of
                                                                True    -> examineFile x
                                                                False   -> putStr "."
                                                            where modtime f = floor $ realToFrac $ modificationTime f
                                                                  dbmod f   = max (fcdAddTime f) (fcdUpdateTime f)
                                            -} 
cleanStereoidDb :: AcidState StereoidDb -> IO ()
cleanStereoidDb sdb = undefined

updateStereoidDb :: AcidState StereoidDb -> IO()
updateStereoidDb sdb = undefined

addToStereoidDb :: FilePath -> AcidState StereoidDb -> IO ()
addToStereoidDb fp sdb = do
    rd <- recurseDir SystemFS fp
    forM_ (filter takeMp3 rd) (doTag sdb)
    where takeMp3 x = (takeExtension x) == ".mp3"
          doTag s x = do
                fc <- getFileCacheData s x 
                case fc of
                    (Just fcd) -> do putStr "o"
                    Nothing    -> examineFile x
                 where examineFile x = do
                        t <- tryJust (guard . isDoesNotExistError) $ ID3.readTag x
                        case t of
                            Left e  -> return ()
                            Right r -> case r of
                                        Nothing  -> putStr $ "x"
                                        Just tag -> do
                                                res <- inAlbumMap s alcd
                                                case res of
                                                    True  -> putStr "."
                                                    False -> putStrLn $ show alcd
                                                where (Just title)  = I.getTitle tag
                                                      (Just artist) = I.getArtist tag
                                                      (Just album)  = I.getAlbum tag
                                                      (Just track)  = I.getTrack tag
                                                      year = getFrameText "TYER" tag
                                                      alcd = AlbumMapData { almdTitle = T.toUpper $ T.pack album
                                                                          , almdArtistName = T.toUpper $ T.pack artist
                                                                          , almdYear = (maybe 0 read year)
                                                                          }
                                                                

main :: IO ()
main = bracket
    (openLocalState (sdbEmpty))
    (closeAcidState)
    (addToStereoidDb "/mnt/emusic")
    
