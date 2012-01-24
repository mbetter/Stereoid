{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}

module DataStructuresInternal where

import Data.Typeable
import qualified Data.ByteString.UTF8 as B

                        
data Song  =  Song  { songID :: Int
                    , songName :: B.ByteString
                    , songTrack :: Int
                    , songAlbumId :: Int
                    , songAlbumTitle :: B.ByteString
                    , songArtistName :: B.ByteString
                    , songDuration :: Int
                    } deriving (Typeable)

data Album =  Album { albumID :: Int
                    , albumTitle :: B.ByteString
                    , albumArtistID :: Int
                    , albumArtistName :: B.ByteString
                    , albumYear :: Int
                    } deriving (Typeable)
