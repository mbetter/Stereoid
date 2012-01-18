
module LastFM.Types where

import qualified Data.Text as T
import qualified Data.ByteString.UTF8 as B

data LastFMResponse = LastFMResponse { album :: Album } deriving (Show)

data Album = Album { name :: T.Text
                   , artist :: Maybe T.Text
                   , mbid :: Maybe T.Text
                   , image :: [Image]
                   , listeners :: Maybe T.Text
                   , playcount :: Maybe T.Text
                   , toptags :: Tags
                   , wiki :: Maybe Wiki
                   } deriving (Show)
data Image = Image { text :: String
                   , size :: T.Text
                   } deriving (Show)
data Tags = Tags { tags :: [Tag] } deriving (Show)
data Tag = Tag { tName :: T.Text
               , tUrl :: B.ByteString
               } deriving (Show)
data Wiki = Wiki { summary :: T.Text
                 , content :: T.Text
                 } deriving (Show)
