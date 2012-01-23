{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module DataStructures where

import Text.JSON
import Happstack.Server        (ToMessage(..))
import Data.Typeable
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString.Lazy.UTF8 as LU


type SessionToken = String
type StereoidId = String
type Timestamp = Integer
type UserName = String

                        
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

instance ToMessage JSValue where
    toContentType _ = C.pack "application/json"
    toMessage val = LU.fromString $ encode val

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

