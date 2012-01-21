{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Types where

import Data.Data               (Data, Typeable)
import Web.Routes              ( PathInfo(..) )
import Web.Routes.TH           (derivePathInfo)

newtype ArtistId 
    = ArtistId { unArtistId :: Int }
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, PathInfo)

newtype AlbumId 
    = AlbumId { unAlbumId :: Int }
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, PathInfo)

newtype SongId 
    = SongId { unSongId :: Int }
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, PathInfo)

