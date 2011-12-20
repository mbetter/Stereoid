{-# LANGUAGE OverloadedStrings #-}
module Id3Test where

import qualified Data.Map as Map
import qualified Data.Text as T
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
import qualified Sound.TagLib as TagLib
import Data.Accessor
import Data.Maybe
import Data.Acid
import Data.Acid.Advanced
import System.Posix.Files


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
                        t <- TagLib.open x 
                        case t of
                            Nothing -> return ()
                            Just r  -> do
                                tt <- TagLib.tag r
                                aa <- TagLib.audioProperties r
                                case (tt,aa) of
                                    (Nothing,_)  -> putStr $ "x"
                                    (_,Nothing)  -> putStr $ "x"
                                    (Just tag,Just ap)  -> do
                                            art <- TagLib.artist tag
                                            alb <- TagLib.album tag
                                            yea <- TagLib.year tag
                                            res <- inAlbumMap s (alcd art alb yea)
                                            case res of
                                                True  -> putStr "."
                                                False -> putStrLn $ show (alcd art alb yea)
                                            where alcd alb art yea= AlbumMapData { almdTitle = T.toUpper $ T.pack alb
                                                                                 , almdArtistName = T.toUpper $ T.pack art
                                                                                 , almdYear = fromInteger yea
                                                                                 }
                                                                

main :: IO ()
main = bracket
    (openLocalState (sdbEmpty))
    (closeAcidState)
    (addToStereoidDb "/root/projects/Stereoid/mp3")
    
