module JsonInstances where

import Text.JSON
import DataStructures

mLookup a as = maybe (fail $ "No such element: " ++ a) return (lookup a as)

-- JSON
instance JSON Session where
    showJSON gd = makeObj
        [ ("sessionToken", showJSON $ sessionToken gd)
        ]
    readJSON (JSObject obj) = let
            jsonObjAssoc = fromJSObject obj
        in do
           sT <- mLookup "sessionToken" jsonObjAssoc >>= readJSON
           return $ Session
               { sessionToken = sT
               }

instance JSON Song where
    showJSON gd = makeObj
        [ ("songID", showJSON $ songID gd)
        , ("songName", showJSON $ songName gd)
        , ("songTrack", showJSON $ songTrack gd)
        , ("songUrl", showJSON $ songUrl gd)
        , ("songAlbumId", showJSON $ songAlbumId gd)
        , ("songAlbumTitle", showJSON $ songAlbumTitle gd)
        , ("songArtistName", showJSON $ songArtistName gd)
        , ("songDuration", showJSON $ songDuration gd)
        ]
    readJSON (JSObject obj) = let
            jsonObjAssoc = fromJSObject obj
        in do
            sI  <- mLookup "songID" jsonObjAssoc >>= readJSON
            sN  <- mLookup "songName" jsonObjAssoc >>= readJSON
            sT  <- mLookup "songTrack" jsonObjAssoc >>= readJSON
            sU  <- mLookup "songUrl" jsonObjAssoc >>= readJSON
            sAl <- mLookup "songAlbumId" jsonObjAssoc >>= readJSON
            sAT <- mLookup "songAlbumTitle" jsonObjAssoc >>= readJSON
            sAN <- mLookup "songArtistName" jsonObjAssoc >>= readJSON
            sD  <- mLookup "songDuration" jsonObjAssoc >>= readJSON
            return $ Song
                { songID = sI
                , songName = sN
                , songTrack = sT
                , songUrl = sU
                , songAlbumId = sAl
                , songAlbumTitle = sAT
                , songArtistName = sAN
                , songDuration = sD
                }

instance JSON Album where
    showJSON gd = makeObj
        [ ("albumID", showJSON $ albumID gd)
        , ("albumTitle", showJSON $ albumTitle gd)
        , ("albumArtistID", showJSON $ albumArtistID gd)
        , ("albumArtistName", showJSON $ albumArtistName gd)
        , ("albumYear", showJSON $ albumYear gd)
        , ("albumArtUrl", showJSON $ albumArtUrl gd)
        , ("albumArtThumbUrl", showJSON $ albumArtThumbUrl gd)
        , ("albumSongsUrl", showJSON $ albumSongsUrl gd)
        , ("albumM3UUrl", showJSON $ albumM3UUrl gd)
        ]
    readJSON (JSObject obj) = let
            jsonObjAssoc = fromJSObject obj
        in do
           alI <- mLookup "albumID" jsonObjAssoc >>= readJSON
           alT <- mLookup "albumTitle" jsonObjAssoc >>= readJSON
           alAi <- mLookup "albumArtistID" jsonObjAssoc >>= readJSON
           alAn <- mLookup "albumArtistName" jsonObjAssoc >>= readJSON
           alYr <- mLookup "albumYear" jsonObjAssoc >>= readJSON
           alAu <- mLookup "albumArtUrl" jsonObjAssoc >>= readJSON
           alAt <- mLookup "albumArtThumbUrl" jsonObjAssoc >>= readJSON
           alSu <- mLookup "albumSongsUrl" jsonObjAssoc >>= readJSON
           alMu <- mLookup "albumM3UUrl" jsonObjAssoc >>= readJSON
           return $ Album 
               { albumID = alI
               , albumTitle = alT
               , albumArtistID = alAi
               , albumArtistName = alAn
               , albumYear = alYr
               , albumArtUrl = alAu
               , albumArtThumbUrl = alAt
               , albumSongsUrl = alSu
               , albumM3UUrl = alMu
               }

instance JSON Artist where
    showJSON gd = makeObj
        [ ("artistID", showJSON $ artistID gd)
        , ("artistName", showJSON $ artistName gd)
        ]
    readJSON (JSObject obj) = let
            jsonObjAssoc = fromJSObject obj
        in do
           aI <- mLookup "artistID" jsonObjAssoc >>= readJSON
           aN <- mLookup "artistName" jsonObjAssoc >>= readJSON
           return $ Artist
               { artistID = aI
               , artistName = aN
               }

