

{-# LANGUAGE OverloadedStrings #-}

module NhlJson where

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

data LastFMResponse = LastFMResponse { album :: Album } deriving (Show)

data Album = Album { name :: T.Text
                   , artist :: T.Text
                   , mbid :: T.Text
                   , image :: [Image]
                   , listeners :: T.Text
                   , playcount :: T.Text
                   , toptags :: Tags
                   , wiki :: Maybe Wiki
                   } deriving (Show)
data Images = Images { images :: [Image] } deriving (Show)
data Image = Image { text :: T.Text
                   , size :: T.Text
                   } deriving (Show)
data Tags = Tags { tags :: [Tag] } deriving (Show)
data Tag = Tag { tName :: T.Text
               , tUrl :: T.Text
               } deriving (Show)
data Wiki = Wiki { summary :: T.Text
                 , content :: T.Text
                 } deriving (Show)

instance FromJSON LastFMResponse where
    parseJSON (Object v) = LastFMResponse <$>
                           v .: "album"
    parseJSON _          = empty
instance FromJSON Album where
    parseJSON (Object v) = Album <$>
                           v .: "name" <*>
                           v .: "artist" <*>
                           v .: "mbid" <*>
                           v .: "image" <*>
                           v .: "listeners" <*>
                           v .: "playcount" <*>
                           v .: "toptags" <*>
                           v .:? "wiki"
    parseJSON _          = empty
instance FromJSON Images where
    parseJSON (Object v) = Images <$>
                           v .: "image"
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

main :: IO ()
main = do
        f <- BL.readFile "test.json"
        let req = decode f :: Maybe LastFMResponse
        print req 
