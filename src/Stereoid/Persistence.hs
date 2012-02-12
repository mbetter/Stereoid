{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings,TypeFamilies, DeriveDataTypeable, TemplateHaskell, TypeSynonymInstances #-}

module Stereoid.Persistence where

import Data.List (isPrefixOf, (\\), foldl')
import Data.Typeable
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Acid
import Data.Acid.Advanced
import Data.SafeCopy
import qualified Data.Trie as Trie
import qualified Data.Trie.Convenience as TC
import System.Random.Shuffle (shuffle')
import System.Random (mkStdGen)
import Data.Maybe (mapMaybe)
import Stereoid.Types hiding (Song(..),Album(..)) 
import Stereoid.Types.Internal
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import qualified Data.ByteString.UTF8 as B
import qualified LastFM.Request as LastFM
deriving instance Typeable1 (Trie.Trie)


$(deriveSafeCopy 0 'base ''JobStatus)
$(deriveSafeCopy 0 'base ''JobData)
$(deriveSafeCopy 0 'base ''SongData)
$(deriveSafeCopy 0 'base ''AlbumData)
$(deriveSafeCopy 0 'base ''ArtistData)
$(deriveSafeCopy 0 'base ''AlbumArtData)
$(deriveSafeCopy 0 'base ''SongCacheData)
$(deriveSafeCopy 0 'base ''AlbumCacheData)
$(deriveSafeCopy 0 'base ''ArtistCacheData)
$(deriveSafeCopy 0 'base ''FileCacheData)
$(deriveSafeCopy 0 'base ''AlbumMapData)
$(deriveSafeCopy 0 'base ''ArtistMapData)
$(deriveSafeCopy 0 'base ''Wiki)
$(deriveSafeCopy 0 'base ''MetaData)
$(deriveSafeCopy 0 'base ''ArtAlt)
$(deriveSafeCopy 0 'base ''ArtAltData)
-- 
$(deriveSafeCopy 0 'base ''JobsDb)
$(deriveSafeCopy 0 'base ''SongDb)
$(deriveSafeCopy 0 'base ''AlbumDb)
$(deriveSafeCopy 0 'base ''ArtistDb)
$(deriveSafeCopy 0 'base ''AlbumArtDb)
$(deriveSafeCopy 0 'base ''ArtistCache)
$(deriveSafeCopy 0 'base ''SongCache)
$(deriveSafeCopy 0 'base ''AlbumCache)
$(deriveSafeCopy 0 'base ''FileCache)
$(deriveSafeCopy 0 'base ''ArtistMap)
$(deriveSafeCopy 0 'base ''AlbumMap)
$(deriveSafeCopy 0 'base ''Stats)
$(deriveSafeCopy 0 'base ''Trie.Trie)
$(deriveSafeCopy 0 'base ''ArtistTrie)
$(deriveSafeCopy 0 'base ''SongTrie)
$(deriveSafeCopy 0 'base ''ArtAltDb)
$(deriveSafeCopy 0 'base ''MetaDataDb)
--
$(deriveSafeCopy 0 'base ''StereoidDb)

cacheToAlbum :: AlbumCacheData -> Int -> Album
cacheToAlbum AlbumCacheData { alcdTitle = title
                            , alcdArtistId = artistid
                            , alcdYear = year
                            , alcdArtistName = name } id = Album { albumID = id
                                                                 , albumTitle = title
                                                                 , albumArtistID = artistid
                                                                 , albumArtistName = name
                                                                 , albumYear = year
                                                                 }

cacheToSong :: SongCacheData -> Int -> Song
cacheToSong (SongCacheData n t _ _ albid albt _ art dur) id = Song id n t albid albt art dur

cacheToArtist :: ArtistCacheData -> Int -> Artist
cacheToArtist ArtistCacheData { arcdName = name } id = Artist { artistID = id
                                                              , artistName = name
                                                              }

-- | Flips an IntMap to a Map, used for generating indexes for reverse lookups
flipIntMap :: Eq a => IntMap.IntMap a -> Map.Map a Int
flipIntMap x = Map.fromAscList $ zip (IntMap.elems x) (IntMap.keys x)

-- | IntMap lookup and return Maybe (key,value)
imQ :: IntMap.IntMap a -> Int -> Maybe (Int, a)
imQ im id = g id $ IntMap.lookup id im
            where g = fmap . (,)

-- | IntMap lookup by a list of keys and return a list of (key,value)
imQs :: [Int] -> IntMap.IntMap a -> [(Int,a)]
imQs i s = mapMaybe (imQ s) i

prefixList' :: [T.Text]
prefixList' = [ "THE "
             , "AN "
             , "A "
             , "The "
             , "the "
             , "An "
             , "an "
             , "a "
             ] -- I'm going to go take a shower now.

stripPrefix :: [T.Text] -> T.Text -> T.Text
stripPrefix (p:ps) str
    | T.isPrefixOf p str = T.drop (T.length p) str
    | otherwise        = stripPrefix ps str
stripPrefix [] str     = str

prefixList :: [String]
prefixList = [ "The "
             , "An "
             , "A "
             ]

splitPrefix :: [String] -> String -> (String, Maybe String)
splitPrefix (p:ps) str
    | isPrefixOf p str = (drop (length p) str, Just $ init p)
    | otherwise        = splitPrefix ps str
splitPrefix [] str     = (str, Nothing)

mkSong :: SongData -> Int -> B.ByteString -> B.ByteString -> Song
mkSong SongData { sodName     =  sn
                , sodTrack    =  st
                , sodAlbumId  = sal
                , sodArtistId = sar
                , sodDuration =  sd
                }  id aln arn = Song { songID         =  id
                                     , songName       =  sn
                                     , songTrack      =  st
                                     , songAlbumId    = sal
                                     , songAlbumTitle = aln
                                     , songArtistName = arn
                                     , songDuration   =  sd
                                     }

-- | Db unwrap function, used with getDb / withDb to pull the structure of a StereoidDb member out of StereoidDb.
-- Similar functions exist for all StereoidDb members.
songDb :: StereoidDb -> IntMap.IntMap SongData
songDb StereoidDb {sdbSongs = SongDb x} = x
albumDb StereoidDb {sdbAlbums = AlbumDb x} = x
artistDb StereoidDb {sdbArtists = ArtistDb x} = x
albumArtDb StereoidDb {sdbArt = AlbumArtDb x} = x
songCache StereoidDb {sdbSongCache = SongCache x} = x
albumCache StereoidDb {sdbAlbumCache = AlbumCache x} = x
albumMap StereoidDb {sdbAlbumMap = AlbumMap x} = x
artistCache StereoidDb {sdbArtistCache = ArtistCache x} = x
artistMap StereoidDb {sdbArtistMap = ArtistMap x} = x
artistTrie StereoidDb {sdbArtistTrie = ArtistTrie x} = x
songTrie StereoidDb {sdbSongTrie = SongTrie x} = x
fileCache StereoidDb {sdbFileCache = FileCache x} = x
artAltDb StereoidDb {sdbArtAlt = ArtAltDb x} = x
metaDataDb StereoidDb {sdbMetaData = MetaDataDb x} = x
stats StereoidDb { sdbStats = x } = x
jobsDb StereoidDb { sdbJobs = JobsDb x } = x

(.:) :: (b -> c) -> (a1 -> a -> b) -> a1 -> a -> c
(.:) = (.) . (.)

-- | Used with an unwrap function to apply a function on the internals of a StereoidDb member.
withDb :: (StereoidDb -> a) -> (a -> b) -> Query StereoidDb b
withDb = asks .: (flip (.))

-- | Used with an unwrap function to pull the entirety of a StereoidDb member.
getDb :: (StereoidDb -> a) -> Query StereoidDb a
getDb = asks

-- | Primary key query function. Tons of these out there.
queryJobsById :: Int -> Query StereoidDb (Maybe (Int,JobData))
queryJobsById = (withDb jobsDb) . (flip imQ) 

queryJobsByIds :: [Int] -> Query StereoidDb [(Int,JobData)]
queryJobsByIds = (withDb jobsDb) . imQs

queryJobs :: Query StereoidDb (IntMap.IntMap JobData)
queryJobs = getDb jobsDb

queryArtAltByAlbumId :: Int -> Query StereoidDb (Maybe (Int,ArtAltData))
queryArtAltByAlbumId = (withDb artAltDb) . (flip imQ) 

queryArtAltByAlbumIds :: [Int] -> Query StereoidDb [(Int,ArtAltData)]
queryArtAltByAlbumIds = (withDb artAltDb) . imQs

queryMetaDataByAlbumId :: Int -> Query StereoidDb (Maybe (Int,MetaData))
queryMetaDataByAlbumId = (withDb metaDataDb) . (flip imQ) 

queryMetaDataByAlbumIds :: [Int] -> Query StereoidDb [(Int,MetaData)]
queryMetaDataByAlbumIds = (withDb metaDataDb) . imQs

querySongBySongId :: Int -> Query StereoidDb (Maybe (Int,SongData))
querySongBySongId = (withDb songDb) . (flip imQ) 

querySongsBySongIds :: [Int] -> Query StereoidDb [(Int,SongData)]
querySongsBySongIds = (withDb songDb) . imQs

-- | Takes a record accessor and turns it into a foreign key predicate.
fKey :: Eq b => (a -> b) -> b -> a -> Bool
fKey f b a = (f a) == b

-- | Filters an IntMap by a predicate and returns it as a (key,value) list.
imFilterList :: (a -> Bool) -> IntMap.IntMap a -> [(Int,a)]
imFilterList = IntMap.toList .: IntMap.filter

-- | Compose this function with a foreign key funcition to get a foreign key song query function, ex:
-- @
--   querySongsByForeignKey . (fKey sodAlbumId)
-- @
querySongsByForeignKey :: (SongData -> Bool) -> Query StereoidDb [(Int,SongData)]
querySongsByForeignKey = (withDb songDb) . imFilterList

-- | Foreign key query function. Common functions have their own indices built so these shouldn't have to 
-- be used all that much.
querySongsByAlbumId :: Int -> Query StereoidDb [(Int,SongData)]
querySongsByAlbumId = querySongsByForeignKey . (fKey sodAlbumId)

querySongsByArtistId :: Int -> Query StereoidDb [(Int,SongData)]
querySongsByArtistId = querySongsByForeignKey . (fKey sodArtistId)

querySongCacheBySongIds :: [Int] -> Query StereoidDb [(Int,SongCacheData)]
querySongCacheBySongIds = (withDb songCache) . imQs

queryAlbumCacheByAlbumIds :: [Int] -> Query StereoidDb [(Int,AlbumCacheData)]
queryAlbumCacheByAlbumIds = (withDb albumCache) . imQs

queryAlbumsByAlbumIds :: [Int] -> Query StereoidDb [(Int,AlbumData)]
queryAlbumsByAlbumIds = (withDb albumDb) . imQs

querySongCacheBySongId :: Int -> Query StereoidDb (Maybe (Int,SongCacheData))
querySongCacheBySongId = (withDb songCache) . (flip imQ)

queryAlbumCacheByAlbumId :: Int -> Query StereoidDb (Maybe (Int,AlbumCacheData))
queryAlbumCacheByAlbumId = (withDb albumCache) . (flip imQ)

queryAlbumByAlbumId :: Int -> Query StereoidDb (Maybe (Int,AlbumData))
queryAlbumByAlbumId = (withDb albumDb) . (flip imQ)

queryArtistCacheByArtistId :: Int -> Query StereoidDb (Maybe (Int,ArtistCacheData))
queryArtistCacheByArtistId = (withDb artistCache) . (flip imQ)

queryArtistCacheByArtistIds :: [Int] -> Query StereoidDb [(Int,ArtistCacheData)]
queryArtistCacheByArtistIds = (withDb artistCache) . imQs

queryArtByAlbumId :: Int -> Query StereoidDb (Maybe (Int,AlbumArtData))
queryArtByAlbumId = (withDb albumArtDb) . (flip imQ)

queryArtistByArtistId :: Int -> Query StereoidDb (Maybe (Int,ArtistData))
queryArtistByArtistId = (withDb artistDb) . (flip imQ)

queryArtistsByArtistIds :: [Int] -> Query StereoidDb [(Int,ArtistData)]
queryArtistsByArtistIds = (withDb artistDb) . imQs

queryArtistCache :: Query StereoidDb (IntMap.IntMap ArtistCacheData)
queryArtistCache = getDb artistCache

queryArtists:: Query StereoidDb (IntMap.IntMap ArtistData)
queryArtists = getDb artistDb

queryArtAlt:: Query StereoidDb (IntMap.IntMap ArtAltData)
queryArtAlt = getDb artAltDb

queryMetaData:: Query StereoidDb (IntMap.IntMap MetaData)
queryMetaData = getDb metaDataDb

querySongs:: Query StereoidDb (IntMap.IntMap SongData)
querySongs = getDb songDb

querySongCache:: Query StereoidDb (IntMap.IntMap SongCacheData)
querySongCache = getDb songCache

queryAlbumCache:: Query StereoidDb (IntMap.IntMap AlbumCacheData)
queryAlbumCache = getDb albumCache

queryAlbums:: Query StereoidDb (IntMap.IntMap AlbumData)
queryAlbums = getDb albumDb

queryArtistMap:: ArtistMapData -> Query StereoidDb (Maybe Int)
queryArtistMap = (withDb artistMap) . Map.lookup

queryElemArtistTrie:: B.ByteString -> Query StereoidDb (Maybe [Int])
queryElemArtistTrie = (withDb artistTrie) . Trie.lookup

queryElemSongTrie:: B.ByteString -> Query StereoidDb (Maybe [Int])
queryElemSongTrie = (withDb songTrie) . Trie.lookup

querySongTrie:: B.ByteString -> Query StereoidDb (Trie.Trie [Int])
querySongTrie = (withDb songTrie) . Trie.submap

queryArtistTrie:: B.ByteString -> Query StereoidDb (Trie.Trie [Int])
queryArtistTrie = (withDb artistTrie) . Trie.submap

queryAlbumMap:: AlbumMapData -> Query StereoidDb (Maybe Int)
queryAlbumMap = (withDb albumMap) . Map.lookup

queryFileCache :: B.ByteString -> Query StereoidDb (Maybe FileCacheData)
queryFileCache = (withDb fileCache) . Map.lookup

queryAlbumArt :: Query StereoidDb (IntMap.IntMap AlbumArtData)
queryAlbumArt = getDb albumArtDb

queryFiles :: Query StereoidDb (Map.Map B.ByteString FileCacheData)
queryFiles = getDb fileCache

queryStats :: Query StereoidDb Stats
queryStats = getDb stats

updateStats :: Stats -> Update StereoidDb ()
updateStats s = do db <- get
                   put (db { sdbStats = s }) 

insertArtAltData :: Int -> ArtAltData -> Update StereoidDb ()
insertArtAltData key value
    = do db <- get
         let (ArtAltDb songs) = sdbArtAlt db
         put (db { sdbArtAlt = ArtAltDb (IntMap.insert key value songs) })

insertMetaData :: Int -> MetaData -> Update StereoidDb ()
insertMetaData key value
    = do db <- get
         let (MetaDataDb songs) = sdbMetaData db
         put (db { sdbMetaData = MetaDataDb (IntMap.insert key value songs) })

insertJobData :: Int -> JobData -> Update StereoidDb ()
insertJobData key value
    = do db <- get
         let (JobsDb jobs) = sdbJobs db
         put (db { sdbJobs = JobsDb (IntMap.insert key value jobs) })

insertSongData :: Int -> SongData -> Update StereoidDb ()
insertSongData key value
    = do db <- get
         let (SongDb songs) = sdbSongs db
         put (db { sdbSongs = SongDb (IntMap.insert key value songs) })

insertAlbumData :: Int -> AlbumData -> Update StereoidDb ()
insertAlbumData key value
    = do db <- get
         let (AlbumDb albums) = sdbAlbums db
         put (db { sdbAlbums  = AlbumDb (IntMap.insert key value albums) })

insertAlbumArtData :: Int -> AlbumArtData -> Update StereoidDb ()
insertAlbumArtData key value
    = do db <- get
         let (AlbumArtDb albums) = sdbArt db
         put (db { sdbArt  = AlbumArtDb (IntMap.insert key value albums) })

insertArtistData :: Int -> ArtistData -> Update StereoidDb ()
insertArtistData key value
    = do db <- get
         let (ArtistDb artists) = sdbArtists db
         put (db { sdbArtists   = ArtistDb (IntMap.insert key value artists) })

insertArtistCacheData :: Int -> ArtistCacheData -> Update StereoidDb ()
insertArtistCacheData key value
    = do db <- get
         let (ArtistCache artistcache) = sdbArtistCache db
         put (db { sdbArtistCache = ArtistCache (IntMap.insert key value artistcache) })

insertSongCacheData :: Int -> SongCacheData -> Update StereoidDb ()
insertSongCacheData key value
    = do db <- get
         let (SongCache songcache) = sdbSongCache db
         put (db { sdbSongCache = SongCache (IntMap.insert key value songcache) })

insertAlbumCacheData :: Int -> AlbumCacheData -> Update StereoidDb ()
insertAlbumCacheData key value
    = do db <- get
         let (AlbumCache albumcache) = sdbAlbumCache db
         put (db { sdbAlbumCache = AlbumCache (IntMap.insert key value albumcache) })

insertFileCacheData :: B.ByteString -> FileCacheData -> Update StereoidDb ()
insertFileCacheData key value
    = do db <- get
         let (FileCache filecache) = sdbFileCache db
         put (db { sdbFileCache = FileCache (Map.insert key value filecache) })

insertJobsDb :: JobsDb -> Update StereoidDb ()
insertJobsDb value
    = do db <- get
         put (db { sdbJobs = value })

insertSongTrie :: SongTrie -> Update StereoidDb ()
insertSongTrie value
    = do db <- get
         put (db { sdbSongTrie = value })

insertKeySongTrie :: B.ByteString -> [Int] -> Update StereoidDb ()
insertKeySongTrie key value
    = do db <- get
         let (SongTrie songtrie) = sdbSongTrie db
         put (db { sdbSongTrie = SongTrie (Trie.insert key value songtrie)})

updateKeyArtistTrie :: B.ByteString -> [Int] -> Update StereoidDb ()
updateKeyArtistTrie key value
    = do db <- get
         let (ArtistTrie artisttrie) = sdbArtistTrie db
         put (db { sdbArtistTrie = ArtistTrie (Trie.insert key value artisttrie)})

newKeyArtistTrie :: B.ByteString -> Update StereoidDb ()
newKeyArtistTrie key
    = do db <- get
         let (ArtistTrie artisttrie) = sdbArtistTrie db
         put (db { sdbArtistTrie = ArtistTrie (Trie.insert key [] artisttrie)})

insertArtistTrie:: ArtistTrie -> Update StereoidDb ()
insertArtistTrie value
    = do db <- get
         put (db { sdbArtistTrie = value })

insertArtistMap:: ArtistMap -> Update StereoidDb ()
insertArtistMap value
    = do db <- get
         put (db { sdbArtistMap = value })

insertArtistMapData :: ArtistMapData -> Int -> Update StereoidDb ()
insertArtistMapData key value
    = do db <- get
         let (ArtistMap artistcache) = sdbArtistMap db
         put (db { sdbArtistMap = ArtistMap (Map.insert key value artistcache) })

insertAlbumArtDb:: AlbumArtDb -> Update StereoidDb ()
insertAlbumArtDb value
    = do db <- get
         put (db { sdbArt = value })

insertSongCache:: SongCache -> Update StereoidDb ()
insertSongCache value
    = do db <- get
         put (db { sdbSongCache = value })

insertAlbumMap:: AlbumMap -> Update StereoidDb ()
insertAlbumMap value
    = do db <- get
         put (db { sdbAlbumMap = value })

insertAlbumMapData :: AlbumMapData -> Int -> Update StereoidDb ()
insertAlbumMapData key value
    = do db <- get
         let (AlbumMap albumcache) = sdbAlbumMap db
         put (db { sdbAlbumMap = AlbumMap (Map.insert key value albumcache) })

$(makeAcidic ''StereoidDb ['insertSongData
                          ,'insertAlbumData
                          ,'insertAlbumArtData
                          ,'insertAlbumArtDb
                          ,'insertSongCacheData
                          ,'insertSongCache
                          ,'insertJobData
                          ,'insertJobsDb
                          ,'insertAlbumCacheData
                          ,'insertAlbumMapData
                          ,'insertAlbumMap
                          ,'insertArtAltData
                          ,'insertMetaData
                          ,'insertArtistData
                          ,'insertKeySongTrie
                          ,'newKeyArtistTrie
                          ,'updateKeyArtistTrie
                          ,'insertArtistCacheData
                          ,'insertArtistMapData
                          ,'insertArtistMap
                          ,'insertArtistTrie
                          ,'insertSongTrie
                          ,'insertFileCacheData
                          ,'queryFileCache
                          ,'queryFiles
                          ,'queryJobsById
                          ,'queryJobsByIds
                          ,'queryJobs
                          ,'querySongBySongId
                          ,'querySongsBySongIds
                          ,'querySongsByAlbumId
                          ,'querySongsByArtistId
                          ,'querySongCacheBySongId
                          ,'querySongCacheBySongIds
                          ,'queryAlbumCacheByAlbumId
                          ,'queryAlbumCacheByAlbumIds
                          ,'queryArtByAlbumId
                          ,'queryAlbumByAlbumId
                          ,'queryAlbumsByAlbumIds
                          ,'queryMetaDataByAlbumId
                          ,'queryArtAltByAlbumId
                          ,'queryMetaDataByAlbumIds
                          ,'queryArtAltByAlbumIds
                          ,'queryAlbums
                          ,'queryAlbumArt
                          ,'queryAlbumMap
                          ,'queryArtistMap
                          ,'queryArtistTrie
                          ,'queryElemSongTrie
                          ,'queryElemArtistTrie
                          ,'querySongTrie
                          ,'querySongCache
                          ,'queryAlbumCache
                          ,'querySongs
                          ,'queryArtists
                          ,'queryArtistCache
                          ,'queryArtistCacheByArtistId
                          ,'queryArtistCacheByArtistIds
                          ,'queryArtistByArtistId
                          ,'queryArtistsByArtistIds
                          ,'queryStats
                          ,'updateStats
                          ])

getFreeSongId :: (Monad m, MonadIO m) => AcidState StereoidDb -> m Int
getFreeSongId acid = do
    qr <- query' acid (QuerySongs)
    let keys = IntMap.keys qr
    return $ head $ [1..] \\ keys

getFreeArtistId :: (Monad m, MonadIO m) => AcidState StereoidDb -> m Int
getFreeArtistId acid = do
    qr <- query' acid (QueryArtists)
    let keys = IntMap.keys qr
    return $ head $ [1..] \\ keys

getFreeAlbumId :: (Monad m, MonadIO m) => AcidState StereoidDb -> m Int
getFreeAlbumId acid = do
    qr <- query' acid (QueryAlbums)
    let keys = IntMap.keys qr
    return $ head $ [1..] \\ keys

getFreeJobId :: (Monad m, MonadIO m) => AcidState StereoidDb -> m Int
getFreeJobId acid = do
    qr <- query' acid (QueryJobs)
    let keys = IntMap.keys qr
    return $ head $ [1..] \\ keys

getJob :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> m (Maybe Job)
getJob acid id = do 
    qr <- query' acid (QueryJobsById id)
    return $ fmap ((Job id) . snd) qr

getJobs :: (Monad m, MonadIO m) => AcidState StereoidDb -> m [Job]
getJobs acid = do
    jobs <- query' acid (QueryJobs)
    return $ map f (IntMap.toList jobs)
        where f (id, job) = Job id job

getSong :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> m (Maybe Song)
getSong acid id = do 
    qr <- query' acid (QuerySongBySongId id)
    case qr of
        Nothing       -> return Nothing
        Just (_,song) -> do let (albumid, artistid) = (sodAlbumId song, sodArtistId song)
                            Just (_,album) <- query' acid (QueryAlbumByAlbumId albumid)
                            Just (_,artist) <- query' acid (QueryArtistByArtistId artistid)
                            return $ Just $ mkSong song id (aldTitle album) (ardName artist)

getSongFile :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> m (Maybe B.ByteString)
getSongFile acid id = do 
    qr <- query' acid (QuerySongBySongId id)
    return $ fmap (sodFile . snd) qr

getAlbum :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> m (Maybe Album)
getAlbum acid id = do 
    qr <- query' acid (QueryAlbumCacheByAlbumId id)
    return $ fmap ((flip cacheToAlbum $ id) . snd) qr

getAlbumCache :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> m (Maybe AlbumCacheData)
getAlbumCache acid id = do
    qr <- query' acid (QueryAlbumCacheByAlbumId id)
    return $ fmap snd qr

getArtistCache :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> m (Maybe ArtistCacheData)
getArtistCache acid id = do
    qr <- query' acid (QueryArtistCacheByArtistId id)
    return $ fmap snd qr

getArtist :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> m (Maybe Artist)
getArtist acid id = do 
    qr <- query' acid (QueryArtistCacheByArtistId id)
    case qr of
        Nothing        -> return Nothing
        Just (_,album) -> return $ Just $ cacheToArtist album id

getArtAlts :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> m (Maybe [ArtAlt])
getArtAlts acid id = do
    qr <- query' acid (QueryArtAltByAlbumId id)
    case qr of
        Nothing         -> return Nothing
        Just (_,(ArtAltData arts))   -> return $ Just arts

getMetaData :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> m (Maybe MetaData)
getMetaData acid id = do
    qr <- query' acid (QueryMetaDataByAlbumId id)
    case qr of
        Nothing         -> return Nothing
        Just (_,md)     -> return $ Just md
                                
-- | Modify album art data in acid store to include thumbnail. If record does not exist, uses thumbnail as art as well.
addThumb :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> B.ByteString -> String -> m ()
addThumb acid id mime file = do
    qr <- query' acid (QueryArtByAlbumId id)
    case qr of
        Nothing  -> update' acid (InsertAlbumArtData id (AlbumArtData mime file (Just mime) (Just file) )) 
        Just (_,aad) -> update' acid (InsertAlbumArtData id (aad {aadThumbMime = Just mime, aadThumbFile = Just file}) ) 
                                    
-- | Modify album art data in acid store. If thumbnail does not currently exist, sets it to Nothing.
addArt :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> B.ByteString -> String -> m ()
addArt acid id mime file = do
    qr <- query' acid (QueryArtByAlbumId id)
    case qr of
        Nothing  -> update' acid (InsertAlbumArtData id (AlbumArtData mime file Nothing Nothing) ) 
        Just (_,aad) -> update' acid (InsertAlbumArtData id (aad {aadMime = mime, aadArtFile = file}) ) 

getArtData :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> m (Maybe AlbumArtData)
getArtData acid id = do 
    qr <- query' acid (QueryArtByAlbumId id)
    case qr of
        Nothing        -> return Nothing
        Just (_,artdata) -> return $ Just artdata

-- | Retrieves album art data from acid store, returning Maybe (MIME :: Bytestring, FilePath :: String)
getArt :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> m (Maybe (B.ByteString, String))
getArt acid id = do 
    qr <- query' acid (QueryArtByAlbumId id)
    case qr of
        Nothing        -> return Nothing
        Just (_,artdata) -> return $ Just $ (aadMime artdata,aadArtFile artdata)

-- | Retrieves album thumbnail data from acid store, returning Maybe (MIME :: Bytestring, FilePath :: String)
getThumb :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> m (Maybe (B.ByteString, String))
getThumb acid id = do 
    qr <- query' acid (QueryArtByAlbumId id)
    case qr of
        Nothing        -> return Nothing
        Just (_,artdata) -> return $ f artdata 
                            where f AlbumArtData { aadThumbMime = Nothing } = Nothing
                                  f AlbumArtData { aadThumbFile = Nothing } = Nothing
                                  f AlbumArtData { aadThumbMime = Just mime, aadThumbFile = Just file } = Just (mime,file)

-- | Retrives Map FilePath :: Bytestring FileCacheData
getFileCache :: (Monad m, MonadIO m) => AcidState StereoidDb -> m (Map.Map B.ByteString FileCacheData)
getFileCache acid = query' acid (QueryFiles)

getFileCacheData :: (Monad m, MonadIO m) => AcidState StereoidDb -> B.ByteString -> m (Maybe FileCacheData)
getFileCacheData acid key = query' acid (QueryFileCache key)

getSongsByAlbumId :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> m [Song]
getSongsByAlbumId acid id = do 
    qr <- query' acid (QueryAlbumCacheByAlbumId id)
    case qr of
        Nothing         -> return []
        Just (aid, acd) -> do 
            songs <- query' acid (QuerySongsBySongIds (alcdSongIds acd)) 
            return $ map (f acd) songs 
                where f alb (sid,sdat) = mkSong sdat sid (alcdTitle acd) (alcdArtistName acd) 

getAlbumsRandom :: (Monad m, MonadIO m) => AcidState StereoidDb -> (Int,Int) -> Int -> m [Album]
getAlbumsRandom acid ol seed = do
    stats <- query' acid (QueryStats)
    getAlbumsSortedBy acid (sortRandom seed (statsAlbumCount stats)) ol

-- |sortRandom takes Seed :: Int and Count :: Int as add'l parameters.
sortRandom :: Int -> Int -> [(Int,a)] -> [(Int,a)]
sortRandom seed count albums = shuffle' albums count (mkStdGen seed)

-- | Get list of albums sorted by an arbitray function.
-- Sort functions, in general, should be functions with type [(Int,a)] -> [(Int,a)]
getAlbumsSortedBy :: (Monad m, MonadIO m) => AcidState StereoidDb -> ([(Int,AlbumCacheData)] -> [(Int,AlbumCacheData)]) -> (Int,Int) -> m [Album]
getAlbumsSortedBy acid sort (offset,limit) = do 
    albums <- query' acid (QueryAlbumCache)
    return $ map f (take limit $ drop offset $ sort $ IntMap.toList albums)
        where f (id, alb) = cacheToAlbum alb id

getSongs :: (Monad m, MonadIO m) => AcidState StereoidDb -> (Int,Int) -> m [Song]
getSongs acid (offset,limit) = do 
    songs <- query' acid (QuerySongCache)
    return $ map f (take limit $ drop offset $ IntMap.toList songs)
        where f (id, cd) = cacheToSong cd id

getAlbums :: (Monad m, MonadIO m) => AcidState StereoidDb -> (Int,Int) -> m [Album]
getAlbums acid (offset,limit) = do 
    albums <- query' acid (QueryAlbumCache)
    return $ map f (take limit $ drop offset $ IntMap.toList albums)
        where f (id, alb) = cacheToAlbum alb id

getArtists :: (Monad m, MonadIO m) => AcidState StereoidDb -> m [Artist]
getArtists acid = do 
    artists <- query' acid (QueryArtistCache)
    return $ map f (IntMap.toList artists)
        where f (id, art) = cacheToArtist art id

getAlbumsByArtistId :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> m [Album]
getAlbumsByArtistId acid id = do 
    qr <- query' acid (QueryArtistCacheByArtistId id)
    case qr of
        Nothing         -> return []
        Just (_, acd) -> do 
            albums <- query' acid (QueryAlbumCacheByAlbumIds (arcdAlbumIds acd))
            return $ map (f) albums
                where f (i,d)  = cacheToAlbum d i

insertRowSongDb :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> SongData -> m ()
insertRowSongDb acid id ad = update' acid (InsertSongData id ad)

insertRowSongCache :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> SongCacheData -> m ()
insertRowSongCache acid id ad = update' acid (InsertSongCacheData id ad)

insertRowAlbumCache :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> AlbumCacheData -> m ()
insertRowAlbumCache acid id ad = update' acid (InsertAlbumCacheData id ad)

insertRowFileCache :: (Monad m, MonadIO m) => AcidState StereoidDb -> B.ByteString -> FileCacheData -> m ()
insertRowFileCache acid id ad = update' acid (InsertFileCacheData id ad)

insertRowArtAltDb :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> ArtAltData -> m ()
insertRowArtAltDb acid id ad = update' acid (InsertArtAltData id ad)

insertRowMetaDataDb :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> MetaData -> m ()
insertRowMetaDataDb acid id ad = update' acid (InsertMetaData id ad)

insertRowAlbumArtDb :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> AlbumArtData -> m ()
insertRowAlbumArtDb acid id ad = update' acid (InsertAlbumArtData id ad)

insertRowAlbumDb :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> AlbumData -> m ()
insertRowAlbumDb acid id ad = update' acid (InsertAlbumData id ad)

insertRowArtistCache :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> ArtistCacheData -> m ()
insertRowArtistCache acid id ad = update' acid (InsertArtistCacheData id ad)

insertRowArtistDb :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> ArtistData -> m ()
insertRowArtistDb acid id ad = update' acid (InsertArtistData id ad)

insertRowAlbumMap :: (Monad m, MonadIO m) => AcidState StereoidDb -> AlbumMapData -> Int -> m ()
insertRowAlbumMap acid md id = update' acid (InsertAlbumMapData md id)

insertRowArtistMap :: (Monad m, MonadIO m) => AcidState StereoidDb -> ArtistMapData -> Int -> m ()
insertRowArtistMap acid md id = update' acid (InsertArtistMapData md id)

insertKeyArtistTrie :: (Monad m, MonadIO m) => AcidState StereoidDb -> B.ByteString -> m ()
insertKeyArtistTrie acid ad = update' acid (NewKeyArtistTrie ad)

insertRowJobsDb :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> JobData -> m ()
insertRowJobsDb acid id ad = update' acid (InsertJobData id ad)

addToArtistTrie :: (Monad m, MonadIO m) => AcidState StereoidDb -> B.ByteString -> Int -> m ()
addToArtistTrie acid key val = do
    qr <- query' acid (QueryElemArtistTrie key)
    let ids = case qr of
                  Nothing -> [val]
                  Just x  -> val:x
    update' acid (UpdateKeyArtistTrie key ids)

addToSongTrie :: (Monad m, MonadIO m) => AcidState StereoidDb -> B.ByteString -> Int -> m ()
addToSongTrie acid key val = do
    qr <- query' acid (QueryElemSongTrie key)
    let ids = case qr of
                  Nothing -> [val]
                  Just x  -> val:x
    update' acid (InsertKeySongTrie key ids)

updateJobStatus :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> JobStatus -> m ()
updateJobStatus acid id st = do
    qr <- query' acid (QueryJobsById id)
    case qr of
        Nothing      -> return ()
        Just (_,job) -> update' acid (InsertJobData id (setStatus job st)) 

updateJobCount :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> Int -> m ()
updateJobCount acid id st = do
    qr <- query' acid (QueryJobsById id)
    case qr of
        Nothing      -> return ()
        Just (_,job) -> update' acid (InsertJobData id (changeCount job st)) 
{-
updateAlbumInfoFromLastFm :: AcidState StereoidDb -> Int -> IO ()
updateAlbumInfoFromLastFm acid id = do
    qr <- query acid (QueryAlbumCacheByAlbumId id)
    case qr of
        Nothing        -> return ()
        Just (_,album) -> do 
            lr <- LastFM.getAlbumInfo (E.decodeUtf8 $ alcdArtistName album) (E.decodeUtf8 $ alcdTitle album)
            case lr of
                Nothing       -> return ()
                Just ((ArtAltData x),md) -> do
                    altd <- query acid (QueryArtAltByAlbumId id)
                    case altd of 
                        Nothing                    -> update acid (InsertArtAltData id (ArtAltData x))
                        Just (_,(ArtAltData alts)) -> update acid (InsertArtAltData id (ArtAltData $ alts ++ x ))
                    update acid (InsertMetaData id md)

testL :: AcidState StereoidDb -> Int -> IO ()
testL acid id = do
    qr <- query acid (QueryAlbumCacheByAlbumId id)
    case qr of
        Nothing        -> return ()
        Just (_,album) -> do 
            lr <- LastFM.getAlbumInfo (E.decodeUtf8 $ alcdArtistName album) (E.decodeUtf8 $ alcdTitle album)
            print $ show lr

getLastFmInfo :: AcidState StereoidDb -> Int -> IO (Maybe (ArtAltData, MetaData))
getLastFmInfo acid id = do
    altd <- query acid (QueryArtAltByAlbumId id)
    md <- query acid (QueryMetaDataByAlbumId id)
    case (altd,md) of
        (_, Nothing) -> return Nothing
        (Nothing, _) -> return Nothing
        ((Just (_,ad)),(Just (_,m))) -> return $ Just (ad,m)
 -}   

buildArtistMap :: (Monad m, MonadIO m) => AcidState StereoidDb -> m ()
buildArtistMap acid = do
    artists <- query' acid (QueryArtistCache)
    update' acid (InsertArtistMap $ ArtistMap (Map.mapKeys f $ flipIntMap artists))
            where f ArtistCacheData { arcdName = name } = ArtistMapData { armdName = T.toUpper $ E.decodeUtf8 name } 

buildSongTrie :: (Monad m, MonadIO m) => AcidState StereoidDb -> m ()
buildSongTrie acid = do
    songs <- query' acid (QuerySongs)
    update' acid (InsertSongTrie $ SongTrie $ TC.fromListWith (++) $ zip (map f (IntMap.elems songs)) (map (:[]) (IntMap.keys songs)))
    where f = E.encodeUtf8 . (stripPrefix prefixList') . T.toUpper . E.decodeUtf8 . sodName


filterSongTrie :: (Monad m, MonadIO m) => AcidState StereoidDb -> B.ByteString -> m [Song]
filterSongTrie acid tx = do
    let g = E.encodeUtf8 . (stripPrefix prefixList') . T.toUpper . E.decodeUtf8 
    tr <- query' acid (QuerySongTrie (g tx))
    albums <- query' acid (QuerySongCacheBySongIds (concat $ Trie.elems tr))
    return $ map (f) albums
             where f (i,d)  = cacheToSong d i

buildArtistTrie :: (Monad m, MonadIO m) => AcidState StereoidDb -> m ()
buildArtistTrie acid = do
    artists <- query' acid (QueryArtistCache)
    update' acid (InsertArtistTrie $ ArtistTrie $ Trie.fromList $ zip (map f (IntMap.elems artists)) (map arcdAlbumIds (IntMap.elems artists)))
    where f = E.encodeUtf8 . (stripPrefix prefixList') . T.toUpper . E.decodeUtf8 . arcdName

filterArtistTrie :: (Monad m, MonadIO m) => AcidState StereoidDb -> B.ByteString -> m [Album]
filterArtistTrie acid tx = do
    let g = E.encodeUtf8 . (stripPrefix prefixList') . T.toUpper . E.decodeUtf8 
    tr <- query' acid (QueryArtistTrie (g tx))
    albums <- query' acid (QueryAlbumCacheByAlbumIds (concat $ Trie.elems tr))
    return $ map (f) albums
             where f (i,d)  = cacheToAlbum d i

buildArtistCache :: (Monad m, MonadIO m) => AcidState StereoidDb -> m ()
buildArtistCache acid = do 
    artists <- query' acid (QueryArtists)
    mapM_ f (IntMap.toList artists)
        where f (aid, ArtistData { ardName = t
                                 , ardSortName = st
                                 })  = do 
                 songs <- query' acid (QuerySongsByArtistId aid)
                 update' acid (InsertArtistCacheData aid (ArtistCacheData { arcdName = t
                                                                          , arcdSortName = st
                                                                          , arcdAlbumIds = (ids songs)
                                                                          }))
                 where ids xs = Set.toList $ Set.fromList $ map sodAlbumId $ snd $ unzip xs

getAlbumMapId :: (Monad m, MonadIO m) => AcidState StereoidDb -> AlbumMapData -> m (Maybe Int)
getAlbumMapId acid key = query' acid (QueryAlbumMap key)

getArtistMapId :: (Monad m, MonadIO m) => AcidState StereoidDb -> ArtistMapData -> m (Maybe Int)
getArtistMapId acid key = query' acid (QueryArtistMap key)

buildAlbumMap :: (Monad m, MonadIO m) => AcidState StereoidDb -> m ()
buildAlbumMap acid = do
    albums <- query' acid (QueryAlbumCache)
    update' acid (InsertAlbumMap $ AlbumMap (Map.mapKeys f $ flipIntMap albums))
            where f AlbumCacheData { alcdTitle = title
                                   , alcdArtistName = art
                                   , alcdYear = year
                                   } = AlbumMapData { almdTitle = T.toUpper $ E.decodeUtf8 title
                                                    , almdArtistName = T.toUpper $ E.decodeUtf8 art
                                                    , almdYear = year
                                                    }


buildSongCache :: (Monad m, MonadIO m) => AcidState StereoidDb -> m ()
buildSongCache acid = do
    qs <- query' acid (QuerySongs)
    qa <- query' acid (QueryAlbums)
    qb <- query' acid (QueryArtists)
    update' acid (InsertSongCache $ SongCache $ fmap (f qa qb) (qs))
    where f albums artists song = sodToScd song (aldTitle $ albums IntMap.! (sodAlbumId song)) (ardName $ artists IntMap.! (sodArtistId song))
    
buildAlbumCache :: (Monad m, MonadIO m) => AcidState StereoidDb -> m ()
buildAlbumCache acid = do 
    albums <- query' acid (QueryAlbums)
    mapM_ f (IntMap.toList albums)
    where f (aid, AlbumData { aldTitle = t
                            , aldSortTitle = st
                            }) = do 
             songs <- query' acid (QuerySongsByAlbumId aid)
             case songs of
                    all@((_,sdat):_)  -> do 
                        (Just (_,artist)) <- query' acid (QueryArtistByArtistId (sodArtistId sdat))
                        update' acid (InsertAlbumCacheData aid (AlbumCacheData { alcdTitle = t
                                                               , alcdSortTitle = st
                                                               , alcdArtistId = (sodArtistId sdat)
                                                               , alcdYear = (sodYear sdat)
                                                               , alcdArtistName = (ardName artist)
                                                               , alcdArtistSortName = (ardSortName artist)
                                                               , alcdSongIds = ((fst . unzip) all)
                                                               }))

lastUpdate :: (Functor m, Monad m, MonadIO m) => AcidState StereoidDb -> m Timestamp
lastUpdate  acid = fmap statsLastUpdate $ query' acid (QueryStats)

songCount :: (Functor m, Monad m, MonadIO m) => AcidState StereoidDb -> m Int
songCount acid = fmap statsSongCount $ query' acid (QueryStats)

artistCount :: (Functor m, Monad m, MonadIO m) => AcidState StereoidDb -> m Int
artistCount acid = fmap statsArtistCount $ query' acid (QueryStats)

albumCount :: (Functor m, Monad m, MonadIO m) => AcidState StereoidDb -> m Int
albumCount acid = fmap statsAlbumCount $ query' acid (QueryStats)

buildStats :: (Monad m, MonadIO m) => AcidState StereoidDb -> m ()
buildStats acid = do
    songs <- query' acid (QuerySongs)
    albums <- query' acid (QueryAlbums)
    artists <- query' acid (QueryArtists)
    files <- query' acid (QueryFiles)
    update' acid (UpdateStats $ stats songs albums files)
            where stats songs albums files = Stats { statsSongCount = IntMap.size songs
                                      , statsAlbumCount = IntMap.size albums
                                      , statsArtistCount = IntMap.size albums
                                      , statsLastUpdate = lastUpdate files
                                      }
                  lastUpdate = foldl' f 0 . Map.elems
                  f x (FileCacheData _ at ut) = max x $ max at ut 

{-
main :: IO ()
main = do
        putStrLn "opening acid-state..."
        sdb <- openLocalState (sdbEmpty)
        putStrLn "building album cache..."
        buildAlbumCache sdb
        putStrLn "building artist cache..."
        buildArtistCache sdb
        putStrLn "closing acid-state..."
        closeAcidState sdb
        putStrLn "done."
-}
