{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module Types where

-- import Text.JSON
import Happstack.Server        (ToMessage(..))
import Data.Typeable
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString.Lazy.UTF8 as LU
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Text as T
import qualified Data.Trie as Trie
import Data.Typeable
import Data.Aeson


type SessionToken = String
type StereoidId = String
type Timestamp = Integer
type UserName = String

emptyMap = Map.empty
                        
data Song  =  Song  { songID :: Int
                    , songName :: B.ByteString
                    , songTrack :: Int
                    , songUrl :: String
                    , songArtUrl :: String
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
                    , albumArtUrl :: String
                    , albumArtThumbUrl :: String
                    , albumSongsUrl :: String
                    , albumM3UUrl :: String
                    } deriving (Typeable)

data Artist = Artist { artistID :: Int
                     , artistName :: B.ByteString
                    } deriving (Typeable)


data JobStatus = JobRunning | JobFinished | JobCancelled | JobError deriving (Show,Eq,Ord,Typeable)


data JobData = Add JobStatus Int |
               Update JobStatus Int |
               Gather JobStatus Int |
               Clean JobStatus Int deriving (Show,Eq,Ord,Typeable)

data Job = Job Int JobData deriving (Typeable)

getStatus :: JobData -> JobStatus
getStatus (Add js _) = js
getStatus (Update js _) = js
getStatus (Gather js _) = js
getStatus (Clean js _) = js

setStatus :: JobData -> JobStatus -> JobData
setStatus (Add _ c) s = (Add s c)
setStatus (Update _ c) s = (Update s c)
setStatus (Gather _ c) s = (Gather s c)
setStatus (Clean _ c) s = (Clean s c)

changeCount :: JobData -> Int -> JobData
changeCount (Add s c) i = (Add s (c + i))
changeCount (Update s c) i = (Update s (c + i))
changeCount (Gather s c) i = (Gather s (c + i))
changeCount (Clean s c) i = (Clean s (c + i))

data Session = Session { sessionToken :: SessionToken }
data Remember = Remember { rRememberToken :: SessionToken
                         , rSessionToken :: SessionToken
                         }

instance ToMessage M3UPlaylist where
    toContentType _ = C.pack "audio/x-mpegurl"
    toMessage (M3UPlaylist val) = LU.fromString val

instance ToMessage Value where
    toContentType _ = C.pack "application/json"
    toMessage val = encode val

newtype M3UPlaylist = M3UPlaylist { unM3UPlaylist :: String }

createM3u :: [Song] -> M3UPlaylist
createM3u xs = M3UPlaylist ("#EXTM3U\n" ++ ( concat $ map createM3ULine xs ) )
                 where createM3ULine Song { songName = title
                                          , songArtistName = artist
                                          , songDuration = time
                                          , songUrl = url } = "\n#EXTINF:" ++ (show time) ++ ", "
                                                                   ++ (B.toString artist) ++ " - "
                                                                   ++ (B.toString title) ++ "\n"
                                                                   ++ url ++ "\n"

data SongData  =  SongData  { sodName     :: B.ByteString
                            , sodTrack    :: Int
                            , sodYear     :: Int
                            , sodFile     :: B.ByteString
                            , sodAlbumId  :: Int
                            , sodArtistId :: Int
                            , sodDuration :: Int
                            } deriving (Eq,Ord,Typeable)

data SongCacheData  =  SongCacheData  { scdName     :: B.ByteString
                                      , scdTrack    :: Int
                                      , scdYear     :: Int
                                      , scdFile     :: B.ByteString
                                      , scdAlbumId  :: Int
                                      , scdAlbumTitle :: B.ByteString
                                      , scdArtistId :: Int
                                      , scdArtistName :: B.ByteString
                                      , scdDuration :: Int
                                      } deriving (Eq,Ord,Typeable)

sodToScd :: SongData -> B.ByteString -> B.ByteString -> SongCacheData
sodToScd (SongData a b c d e f g) alb art = SongCacheData a b c d e alb f art g

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
                                   , fcdAddTime :: Timestamp
                                   , fcdUpdateTime :: Timestamp
                                   } deriving (Eq,Ord, Typeable)

data AlbumMapData = AlbumMapData { almdTitle :: T.Text
                                 , almdArtistName :: T.Text
                                 , almdYear  :: Int
                                 } deriving (Show,Eq,Ord,Typeable)

data ArtistMapData = ArtistMapData { armdName :: T.Text
                                   } deriving (Show,Eq,Ord,Typeable)

data Wiki = Wiki { wSummary :: T.Text
                 , wContent :: T.Text
                 } deriving (Show,Eq,Ord,Typeable)

data MetaData = MetaData { mdMbid :: Maybe T.Text
                         , mdTags :: [T.Text]
                         , mdWiki :: Maybe Wiki
                         } deriving (Show,Eq,Ord,Typeable)

mdEmpty = MetaData Nothing [] Nothing

data ArtAlt = FileArt B.ByteString B.ByteString |
              LastFMArt T.Text B.ByteString deriving (Show,Eq,Ord,Typeable)

data ArtAltData = ArtAltData [ArtAlt] deriving (Show,Eq,Ord,Typeable)




data JobsDb = JobsDb !(IntMap.IntMap JobData) deriving (Typeable)
data SongDb     = SongDb !(IntMap.IntMap     SongData) deriving (Typeable)
data AlbumDb    = AlbumDb !(IntMap.IntMap    AlbumData) deriving (Typeable)
data ArtistDb   = ArtistDb !(IntMap.IntMap   ArtistData) deriving (Typeable)
data AlbumArtDb = AlbumArtDb !(IntMap.IntMap AlbumArtData) deriving (Typeable)
data AlbumCache = AlbumCache !(IntMap.IntMap AlbumCacheData) deriving (Typeable)
data SongCache = SongCache !(IntMap.IntMap SongCacheData) deriving (Typeable)
data ArtistCache = ArtistCache !(IntMap.IntMap ArtistCacheData) deriving (Typeable)
data FileCache  = FileCache !(Map.Map B.ByteString FileCacheData) deriving (Typeable)
data AlbumMap = AlbumMap !(Map.Map AlbumMapData Int) deriving (Typeable)
data ArtistMap = ArtistMap !(Map.Map ArtistMapData Int) deriving (Typeable)
data ArtistTrie = ArtistTrie !(Trie.Trie [Int]) deriving (Typeable)
data SongTrie = SongTrie !(Trie.Trie [Int]) deriving (Typeable)
data MetaDataDb = MetaDataDb !(IntMap.IntMap MetaData) deriving (Typeable)
data ArtAltDb = ArtAltDb !(IntMap.IntMap ArtAltData) deriving (Typeable)


data Stats = Stats { statsArtistCount :: Int
                   , statsAlbumCount :: Int
                   , statsSongCount :: Int
                   , statsLastUpdate :: Timestamp
                   } deriving (Typeable)

sdbStatsEmpty :: Stats
sdbStatsEmpty = Stats { statsArtistCount = 0
                      , statsAlbumCount = 0
                      , statsSongCount = 0
                      , statsLastUpdate = 0
                      }
data StereoidDb = StereoidDb { sdbSongs   ::     SongDb
                             , sdbAlbums  ::    AlbumDb
                             , sdbArtists ::   ArtistDb
                             , sdbArt     :: AlbumArtDb
                             , sdbSongCache :: SongCache
                             , sdbAlbumCache :: AlbumCache
                             , sdbArtistCache :: ArtistCache
                             , sdbFileCache :: FileCache
                             , sdbAlbumMap :: AlbumMap
                             , sdbArtistMap :: ArtistMap
                             , sdbStats       :: Stats
                             , sdbArtistTrie :: ArtistTrie
                             , sdbSongTrie :: SongTrie
                             , sdbArtAlt :: ArtAltDb
                             , sdbMetaData :: MetaDataDb
                             , sdbJobs :: JobsDb
                             } deriving (Typeable)


sdbEmpty :: StereoidDb
sdbEmpty = StereoidDb { sdbSongs = SongDb IntMap.empty
                      , sdbAlbums = AlbumDb IntMap.empty
                      , sdbArtists = ArtistDb IntMap.empty
                      , sdbArt = AlbumArtDb IntMap.empty
                      , sdbSongCache = SongCache IntMap.empty
                      , sdbAlbumCache = AlbumCache IntMap.empty
                      , sdbArtistCache = ArtistCache IntMap.empty
                      , sdbFileCache = FileCache Map.empty
                      , sdbAlbumMap = AlbumMap Map.empty
                      , sdbArtistMap = ArtistMap Map.empty
                      , sdbStats = sdbStatsEmpty
                      , sdbArtistTrie = ArtistTrie Trie.empty
                      , sdbSongTrie = SongTrie Trie.empty
                      , sdbArtAlt = ArtAltDb IntMap.empty
                      , sdbMetaData = MetaDataDb IntMap.empty
                      , sdbJobs = JobsDb IntMap.empty
                      }

