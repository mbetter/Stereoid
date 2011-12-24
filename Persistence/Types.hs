{-# LANGUAGE DeriveDataTypeable #-}

module Persistence.Types where

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.ByteString.UTF8 as B
import qualified Data.Text as T
import qualified DataStructures as DS
import Data.Typeable
import DataStructuresInternal

data SongData  =  SongData  { sodName     :: B.ByteString
                            , sodTrack    :: Int
                            , sodYear     :: Int
                            , sodFile     :: B.ByteString
                            , sodAlbumId  :: Int
                            , sodArtistId :: Int
                            , sodDuration :: Int
                            } deriving (Eq,Ord,Typeable)

data AlbumData =  AlbumData { aldTitle     :: B.ByteString
                            , aldSortTitle :: B.ByteString
                            } deriving (Eq, Ord, Typeable)

data ArtistData = ArtistData { ardName   :: B.ByteString
                             , ardSortName :: B.ByteString
                             } deriving (Eq,Ord,Typeable)

data AlbumArtData = AlbumArtData { aadMime :: B.ByteString
                                 , aadArtFile  :: String
                                 , aadThumbMime :: (Maybe B.ByteString)
                                 , aadThumbFile :: (Maybe String)
                                 } deriving (Eq,Ord,Typeable)

data AlbumCacheData = AlbumCacheData { alcdTitle :: B.ByteString
                                     , alcdSortTitle :: B.ByteString
                                     , alcdArtistId :: Int
                                     , alcdArtistName :: B.ByteString
                                     , alcdArtistSortName :: B.ByteString
                                     , alcdYear :: Int
                                     , alcdSongIds :: [Int]
                                     } deriving (Show,Eq,Ord,Typeable)

data ArtistCacheData = ArtistCacheData { arcdName :: B.ByteString
                                       , arcdSortName :: B.ByteString
                                       , arcdAlbumIds :: [Int]
                                       } deriving (Eq,Ord,Typeable)

data FileCacheData = FileCacheData { fcdSongId :: Int
                                   , fcdAddTime :: DS.Timestamp
                                   , fcdUpdateTime :: DS.Timestamp
                                   } deriving (Eq,Ord, Typeable)

data AlbumMapData = AlbumMapData { almdTitle :: T.Text
                                 , almdArtistName :: T.Text
                                 , almdYear  :: Int
                                 } deriving (Show,Eq,Ord,Typeable)

data ArtistMapData = ArtistMapData { armdName :: T.Text
                                   } deriving (Show,Eq,Ord,Typeable)

data SongDb     = SongDb !(IntMap.IntMap     SongData) deriving (Typeable)
data AlbumDb    = AlbumDb !(IntMap.IntMap    AlbumData) deriving (Typeable)
data ArtistDb   = ArtistDb !(IntMap.IntMap   ArtistData) deriving (Typeable)
data AlbumArtDb = AlbumArtDb !(IntMap.IntMap AlbumArtData) deriving (Typeable)
data AlbumCache = AlbumCache !(IntMap.IntMap AlbumCacheData) deriving (Typeable)
data ArtistCache = ArtistCache !(IntMap.IntMap ArtistCacheData) deriving (Typeable)
data FileCache  = FileCache !(Map.Map B.ByteString FileCacheData) deriving (Typeable)
data AlbumMap = AlbumMap !(Map.Map AlbumMapData Int) deriving (Typeable)
data ArtistMap = ArtistMap !(Map.Map ArtistMapData Int) deriving (Typeable)

data StereoidDb = StereoidDb { sdbSongs   ::     SongDb
                             , sdbAlbums  ::    AlbumDb
                             , sdbArtists ::   ArtistDb
                             , sdbArt     :: AlbumArtDb
                             , sdbAlbumCache :: AlbumCache
                             , sdbArtistCache :: ArtistCache
                             , sdbFileCache :: FileCache
                             , sdbAlbumMap :: AlbumMap
                             , sdbArtistMap :: ArtistMap
                             } deriving (Typeable)


sdbEmpty :: StereoidDb
sdbEmpty = StereoidDb { sdbSongs = (SongDb IntMap.empty)
                      , sdbAlbums = (AlbumDb IntMap.empty)
                      , sdbArtists = (ArtistDb IntMap.empty)
                      , sdbArt = (AlbumArtDb IntMap.empty)
                      , sdbAlbumCache = (AlbumCache IntMap.empty)
                      , sdbArtistCache = (ArtistCache IntMap.empty)
                      , sdbFileCache = (FileCache Map.empty)
                      , sdbAlbumMap = (AlbumMap Map.empty)
                      , sdbArtistMap = (ArtistMap Map.empty)
                      }
