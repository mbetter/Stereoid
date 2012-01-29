{-# LANGUAGE OverloadedStrings #-}
module Stereoid.JsonInstances where

import Data.Aeson
import Stereoid.Types

instance ToJSON Session where
   toJSON (Session s) = object ["sessionToken" .= s]

instance ToJSON Remember where
    toJSON (Remember r s) = object ["sessionToken" .= s,"loginToken" .= r]

instance ToJSON Song where
    toJSON (Song i n t u au ai at an d) = object  
        [ "songID" .= i
        , "songName" .= n
        , "songTrack" .= t
        , "songUrl" .= u
        , "songArtUrl" .= au
        , "songAlbumId" .= ai
        , "songAlbumTitle" .= at
        , "songArtistName" .= an
        , "songDuration" .= d
        ]

instance ToJSON Album where
    toJSON (Album i t ai an y au at su mu) = object
        [ "albumID" .= i
        , "albumTitle" .= t
        , "albumArtistID" .= ai
        , "albumArtistName" .= an
        , "albumYear" .= y
        , "albumArtUrl" .= au
        , "albumArtThumbUrl" .= at
        , "albumSongsUrl" .= su
        , "albumM3UUrl" .= mu
        ]

instance ToJSON Artist where
    toJSON (Artist i n) = object ["artistID" .= i, "artistName" .= n]


instance ToJSON JobStatus where
    toJSON JobRunning = "running"
    toJSON JobCancelled = "cancelled"
    toJSON JobError = "error"
    toJSON JobFinished = "finished"

instance ToJSON Job where
    toJSON (Job i (Add s c)) = object [ "type" .= (String "add"), "id" .= i, "status" .= s, "added" .= c ]
    toJSON (Job i (Update s c)) = object [ "type" .= (String "update"), "id" .= i, "status" .= s, "updated" .= c ]
    toJSON (Job i (Gather s c)) = object [ "type" .= (String "gather"), "id" .= i, "status" .= s, "gathered" .= c ]
    toJSON (Job i (Clean s c)) = object [ "type" .= (String "clean"), "id" .= i, "status" .= s, "cleaned" .= c ]

{-
data JobData = Add JobStatus Int |
               Update JobStatus Int |
               Gather JobStatus Int |
               Clean JobStatus Int deriving (Show,Eq,Ord,Typeable)

data Job = Job Int JobData deriving (Typeable)
data JobStatus = JobRunning | JobFinished | JobCancelled | JobError deriving (Show,Eq,Ord,Typeable)


data JobData = Add JobStatus Int Int Int |
               Update JobStatus Int |
               Gather JobStatus Int |
               Clean JobStatus Int deriving (Show,Eq,Ord,Typeable)
-}
