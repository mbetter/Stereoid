{-# LANGUAGE OverloadedStrings #-}
module JsonInstancesAeson where

import Data.Aeson
import DataStructures

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

