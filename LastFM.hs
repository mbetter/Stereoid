

{-# LANGUAGE OverloadedStrings #-}

module LastFM where

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T



main :: IO ()
main = do
        f <- BL.readFile "test.json"
        let req = decode f :: Maybe LastFMResponse
        print req 
