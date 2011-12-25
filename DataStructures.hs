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

