{-# LANGUAGE OverloadedStrings #-}

module LastFM.JSON where

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import LastFM.Types


instance FromJSON LastFMResponse where
    parseJSON (Object v) = LastFMResponse <$>
                           v .: "album"
    parseJSON _          = empty
instance FromJSON Album where
    parseJSON (Object v) = Album <$>
                           v .: "name" <*>
                           v .:? "artist" <*>
                           v .:? "mbid" <*>
                           v .: "image" <*>
                           v .:? "listeners" <*>
                           v .:? "playcount" <*>
                           v .:? "toptags" <*>
                           v .:? "wiki"
    parseJSON _          = empty
instance FromJSON Image where
    parseJSON (Object v) = Image <$>
                           v .: "#text" <*>
                           v .: "size"
    parseJSON _          = empty
instance FromJSON Wiki where
    parseJSON (Object v) = Wiki <$>
                           v .: "summary" <*>
                           v .: "content"
    parseJSON _          = empty
instance FromJSON Tags where
    parseJSON (Object v) = Tags <$>
                           v .: "tag"
    parseJSON _          = empty
instance FromJSON Tag where
    parseJSON (Object v) = Tag <$>
                           v .: "name" <*>
                           v .: "url"
    parseJSON _          = empty
