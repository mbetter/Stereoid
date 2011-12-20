{-# LANGUAGE OverloadedStrings #-}
module ShowTag where

import Audio.TagLib.TagLib
import System.Environment (getArgs)
import qualified Data.ByteString.UTF8 as B

main :: IO ()
main = do
         args <- getArgs
         tagf <- tagFileOpen $ B.fromString $ head args 
         case tagf of
            Nothing -> putStrLn "error"
            Just fl -> do 
                        ap <- tagFileGetAudioProperties fl
                        case ap of
                            Nothing -> putStrLn "error ap"
                            Just a  -> do
                                mdur <- audioPropertiesGetDuration a
                                putStrLn $ show mdur
