
module LastFM.Types where

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
