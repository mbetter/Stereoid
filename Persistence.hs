{-# LANGUAGE OverloadedStrings,TypeFamilies, DeriveDataTypeable, TemplateHaskell, TypeSynonymInstances #-}

module Persistence where

import Data.List (isPrefixOf, (\\))
import Data.Typeable
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Acid
import Data.Acid.Advanced
import Data.SafeCopy
import Data.Maybe (mapMaybe)
import qualified DataStructures as DS
import DataStructuresInternal
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import qualified Data.ByteString.UTF8 as B

data SongData  =  SongData  { sodName     :: B.ByteString
                            , sodTrack    :: Int
                            , sodYear     :: Int
                            , sodFile     :: B.ByteString
                            , sodAlbumId  :: Int
                            , sodArtistId :: Int
                            , sodDuration :: Int
                            } deriving (Eq,Ord,Typeable)

$(deriveSafeCopy 0 'base ''SongData)

data AlbumData =  AlbumData { aldTitle     :: B.ByteString
                            , aldSortTitle :: B.ByteString
                            } deriving (Eq, Ord, Typeable)

$(deriveSafeCopy 0 'base ''AlbumData)

data ArtistData = ArtistData { ardName   :: B.ByteString
                             , ardSortName :: B.ByteString
                             } deriving (Eq,Ord,Typeable)

$(deriveSafeCopy 0 'base ''ArtistData)

data AlbumArtData = AlbumArtData { aadMime :: B.ByteString
                                 , aadArtFile  :: String
                                 , aadThumbMime :: (Maybe B.ByteString)
                                 , aadThumbFile :: (Maybe String)
                                 } deriving (Eq,Ord,Typeable)

$(deriveSafeCopy 0 'base ''AlbumArtData)

data AlbumCacheData = AlbumCacheData { alcdTitle :: B.ByteString
                                     , alcdSortTitle :: B.ByteString
                                     , alcdArtistId :: Int
                                     , alcdArtistName :: B.ByteString
                                     , alcdArtistSortName :: B.ByteString
                                     , alcdYear :: Int
                                     , alcdSongIds :: [Int]
                                     } deriving (Show,Eq,Ord,Typeable)

$(deriveSafeCopy 0 'base ''AlbumCacheData)

data ArtistCacheData = ArtistCacheData { arcdName :: B.ByteString
                                       , arcdSortName :: B.ByteString
                                       , arcdAlbumIds :: [Int]
                                       } deriving (Eq,Ord,Typeable)

$(deriveSafeCopy 0 'base ''ArtistCacheData)

data FileCacheData = FileCacheData { fcdSongId :: Int
                                   , fcdAddTime :: DS.Timestamp
                                   , fcdUpdateTime :: DS.Timestamp
                                   } deriving (Eq,Ord, Typeable)

$(deriveSafeCopy 0 'base ''FileCacheData)

data AlbumMapData = AlbumMapData { almdTitle :: T.Text
                                 , almdArtistName :: T.Text
                                 , almdYear  :: Int
                                 } deriving (Show,Eq,Ord,Typeable)

$(deriveSafeCopy 0 'base ''AlbumMapData)

data ArtistMapData = ArtistMapData { armdName :: T.Text
                                   } deriving (Show,Eq,Ord,Typeable)

$(deriveSafeCopy 0 'base ''ArtistMapData)

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

cacheToArtist :: ArtistCacheData -> Int -> DS.Artist
cacheToArtist ArtistCacheData { arcdName = name } id = DS.Artist { DS.artistID = id
                                                                 , DS.artistName = name
                                                                 }

data SongDb     = SongDb !(IntMap.IntMap     SongData) deriving (Typeable)
data AlbumDb    = AlbumDb !(IntMap.IntMap    AlbumData) deriving (Typeable)
data ArtistDb   = ArtistDb !(IntMap.IntMap   ArtistData) deriving (Typeable)
data AlbumArtDb = AlbumArtDb !(IntMap.IntMap AlbumArtData) deriving (Typeable)
data AlbumCache = AlbumCache !(IntMap.IntMap AlbumCacheData) deriving (Typeable)
data ArtistCache = ArtistCache !(IntMap.IntMap ArtistCacheData) deriving (Typeable)
data FileCache  = FileCache !(Map.Map B.ByteString FileCacheData) deriving (Typeable)
data AlbumMap = AlbumMap !(Map.Map AlbumMapData Int) deriving (Typeable)
data ArtistMap = ArtistMap !(Map.Map ArtistMapData Int) deriving (Typeable)

$(deriveSafeCopy 0 'base ''SongDb)
$(deriveSafeCopy 0 'base ''AlbumDb)
$(deriveSafeCopy 0 'base ''ArtistDb)
$(deriveSafeCopy 0 'base ''AlbumArtDb)
$(deriveSafeCopy 0 'base ''ArtistCache)
$(deriveSafeCopy 0 'base ''AlbumCache)
$(deriveSafeCopy 0 'base ''FileCache)
$(deriveSafeCopy 0 'base ''ArtistMap)
$(deriveSafeCopy 0 'base ''AlbumMap)

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

$(deriveSafeCopy 0 'base ''StereoidDb)

flipIntMap :: Eq a => IntMap.IntMap a -> Map.Map a Int
flipIntMap x = Map.fromAscList $ zip (IntMap.elems x) (IntMap.keys x)

imQ :: IntMap.IntMap a -> Int -> Maybe (Int, a)
imQ im id = g id $ IntMap.lookup id im
            where g = fmap . (,)

imQs :: [Int] -> IntMap.IntMap a -> [(Int,a)]
imQs i s = mapMaybe (imQ s) i

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

songDb :: StereoidDb -> IntMap.IntMap SongData
songDb StereoidDb {sdbSongs = SongDb x} = x
albumDb StereoidDb {sdbAlbums = AlbumDb x} = x
artistDb StereoidDb {sdbArtists = ArtistDb x} = x
albumArtDb StereoidDb {sdbArt = AlbumArtDb x} = x
albumCache StereoidDb {sdbAlbumCache = AlbumCache x} = x
albumMap StereoidDb {sdbAlbumMap = AlbumMap x} = x
artistCache StereoidDb {sdbArtistCache = ArtistCache x} = x
artistMap StereoidDb {sdbArtistMap = ArtistMap x} = x
fileCache StereoidDb {sdbFileCache = FileCache x} = x

withDb :: (StereoidDb -> a) -> (a -> b) -> Query StereoidDb b
withDb unw f = do db <- ask
                  return $ (f . unw) db

getDb :: (StereoidDb -> a) -> Query StereoidDb a
getDb f = do db <- ask
             return $ f db

querySongBySongId :: Int -> Query StereoidDb (Maybe (Int,SongData))
querySongBySongId = (withDb songDb) . (flip imQ) 

querySongsBySongIds :: [Int] -> Query StereoidDb [(Int,SongData)]
querySongsBySongIds = (withDb songDb) . imQs

fKey :: Eq b => (a -> b) -> b -> a -> Bool
fKey f b a = (f a) == b

imFilterList :: (a -> Bool) -> IntMap.IntMap a -> [(Int,a)]
imFilterList p = IntMap.toList . (IntMap.filter p)

querySongsByForeignKey :: (SongData -> Bool) -> Query StereoidDb [(Int,SongData)]
querySongsByForeignKey p = (withDb songDb) $ (imFilterList $ p)

querySongsByAlbumId :: Int -> Query StereoidDb [(Int,SongData)]
querySongsByAlbumId = querySongsByForeignKey . (fKey sodAlbumId)

querySongsByArtistId :: Int -> Query StereoidDb [(Int,SongData)]
querySongsByArtistId = querySongsByForeignKey . (fKey sodArtistId)

queryAlbumCacheByAlbumIds :: [Int] -> Query StereoidDb [(Int,AlbumCacheData)]
queryAlbumCacheByAlbumIds = (withDb albumCache) . imQs

queryAlbumsByAlbumIds :: [Int] -> Query StereoidDb [(Int,AlbumData)]
queryAlbumsByAlbumIds = (withDb albumDb) . imQs

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

querySongs:: Query StereoidDb (IntMap.IntMap SongData)
querySongs = getDb songDb

queryAlbumCache:: Query StereoidDb (IntMap.IntMap AlbumCacheData)
queryAlbumCache = getDb albumCache

queryAlbums:: Query StereoidDb (IntMap.IntMap AlbumData)
queryAlbums = getDb albumDb

queryArtistMap:: ArtistMapData -> Query StereoidDb (Maybe Int)
queryArtistMap = (withDb artistMap) . Map.lookup

queryAlbumMap:: AlbumMapData -> Query StereoidDb (Maybe Int)
queryAlbumMap = (withDb albumMap) . Map.lookup

queryFileCache :: B.ByteString -> Query StereoidDb (Maybe FileCacheData)
queryFileCache = (withDb fileCache) . Map.lookup

queryFiles :: Query StereoidDb (Map.Map B.ByteString FileCacheData)
queryFiles = getDb fileCache

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

insertArtistMap:: ArtistMap -> Update StereoidDb ()
insertArtistMap value
    = do db <- get
         put (db { sdbArtistMap = value })

insertArtistMapData :: ArtistMapData -> Int -> Update StereoidDb ()
insertArtistMapData key value
    = do db <- get
         let (ArtistMap artistcache) = sdbArtistMap db
         put (db { sdbArtistMap = ArtistMap (Map.insert key value artistcache) })

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
                          ,'insertAlbumCacheData
                          ,'insertAlbumMapData
                          ,'insertAlbumMap
                          ,'insertArtistData
                          ,'insertArtistCacheData
                          ,'insertArtistMapData
                          ,'insertArtistMap
                          ,'insertFileCacheData
                          ,'queryFileCache
                          ,'queryFiles
                          ,'querySongBySongId
                          ,'querySongsBySongIds
                          ,'querySongsByAlbumId
                          ,'querySongsByArtistId
                          ,'queryAlbumCacheByAlbumId
                          ,'queryAlbumCacheByAlbumIds
                          ,'queryArtByAlbumId
                          ,'queryAlbumByAlbumId
                          ,'queryAlbumsByAlbumIds
                          ,'queryAlbums
                          ,'queryAlbumMap
                          ,'queryArtistMap
                          ,'queryAlbumCache
                          ,'querySongs
                          ,'queryArtists
                          ,'queryArtistCache
                          ,'queryArtistCacheByArtistId
                          ,'queryArtistCacheByArtistIds
                          ,'queryArtistByArtistId
                          ,'queryArtistsByArtistIds
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
    case qr of
        Nothing       -> return Nothing
        Just (_,song) -> return $ Just $ (sodFile song) 

getAlbum :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> m (Maybe Album)
getAlbum acid id = do 
    qr <- query' acid (QueryAlbumCacheByAlbumId id)
    case qr of
        Nothing      -> return Nothing
        Just (_,acd) -> return $ Just $ cacheToAlbum acd id

getArtist :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> m (Maybe DS.Artist)
getArtist acid id = do 
    qr <- query' acid (QueryArtistCacheByArtistId id)
    case qr of
        Nothing        -> return Nothing
        Just (_,album) -> return $ Just $ cacheToArtist album id

getArt :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> m (Maybe (B.ByteString, String))
getArt acid id = do 
    qr <- query' acid (QueryArtByAlbumId id)
    case qr of
        Nothing        -> return Nothing
        Just (_,artdata) -> return $ Just $ (aadMime artdata,aadArtFile artdata)

getThumb :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> m (Maybe (B.ByteString, String))
getThumb acid id = do 
    qr <- query' acid (QueryArtByAlbumId id)
    case qr of
        Nothing        -> return Nothing
        Just (_,artdata) -> return $ f artdata 
                            where f AlbumArtData { aadThumbMime = Nothing } = Nothing
                                  f AlbumArtData { aadThumbFile = Nothing } = Nothing
                                  f AlbumArtData { aadThumbMime = Just mime, aadThumbFile = Just file } = Just (mime,file)

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

getAlbums :: (Monad m, MonadIO m) => AcidState StereoidDb -> (Int,Int) -> m [Album]
getAlbums acid (offset,limit) = do 
    albums <- query' acid (QueryAlbumCache)
    return $ map f (take limit $ drop offset $ IntMap.toList albums)
        where f (id, alb) = cacheToAlbum alb id

getArtists :: (Monad m, MonadIO m) => AcidState StereoidDb -> m [DS.Artist]
getArtists acid = do 
    artists <- query' acid (QueryArtistCache)
    return $ map f (IntMap.toList artists)
        where f (id, art) = cacheToArtist art id

getAlbumsByArtistId :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> m [Album]
getAlbumsByArtistId acid id = do 
    qr <- query' acid (QueryArtistCacheByArtistId id)
    case qr of
        Nothing         -> return []
        Just (aid, acd) -> do 
            albums <- query' acid (QueryAlbumCacheByAlbumIds (arcdAlbumIds acd))
            return $ map (f) albums
                where f (i,d)  = cacheToAlbum d i

insertRowSongDb :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> SongData -> m ()
insertRowSongDb acid id ad = update' acid (InsertSongData id ad)

insertRowAlbumCache :: (Monad m, MonadIO m) => AcidState StereoidDb -> Int -> AlbumCacheData -> m ()
insertRowAlbumCache acid id ad = update' acid (InsertAlbumCacheData id ad)

insertRowFileCache :: (Monad m, MonadIO m) => AcidState StereoidDb -> B.ByteString -> FileCacheData -> m ()
insertRowFileCache acid id ad = update' acid (InsertFileCacheData id ad)

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

buildArtistMap :: (Monad m, MonadIO m) => AcidState StereoidDb -> m ()
buildArtistMap acid = do
    artists <- query' acid (QueryArtistCache)
    update' acid (InsertArtistMap $ ArtistMap (Map.mapKeys f $ flipIntMap artists))
            where f ArtistCacheData { arcdName = name } = ArtistMapData { armdName = T.toUpper $ E.decodeUtf8 name } 

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
