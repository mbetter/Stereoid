{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Routing where

import Prelude                 hiding (head, id, (.))
import Control.Category        (Category(id, (.)))
import Data.Data               (Data, Typeable)
import Text.Boomerang.TH       (derivePrinterParsers)
import Web.Routes              ( PathInfo(..), RouteT    , showURL , runRouteT
                               , Site(..)    , setDefault, mkSitePI           )
import Web.Routes.TH           (derivePathInfo)
import Web.Routes.Happstack    (implSite)
import Web.Routes.Boomerang    
import Database.HDBC
import Auth
import Happstack.Server        (port      , Response     , ServerPartT, ok           , toResponse
                               ,simpleHTTP, nullConf     , seeOther   , dir          , notFound
                               ,seeOther  , asContentType, serveFile  , ToMessage(..)                                                    )

newtype ArtistId 
    = ArtistId { unArtistId :: Int }
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, PathInfo)

newtype AlbumId 
    = AlbumId { unAlbumId :: Int }
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, PathInfo)

newtype SongId 
    = SongId { unSongId :: Int }
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, PathInfo)

data Sitemap
    = Home
    | Songs
    | Albums
    | Artists
    | ArtistInfo ArtistId
    | ArtistAlbums ArtistId
    | AlbumInfo AlbumId
    | AlbumSongs AlbumId
    | AlbumM3U AlbumId
    | AlbumArt AlbumId
    | AlbumArtThumb AlbumId
    | Sessions
    | Users
    | Stream SongId
      deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePrinterParsers ''Sitemap)

sitemap :: Router Sitemap
sitemap =
    (  rHome
    <> rStream . (lit "stream" </> songId)
    <> rSessions . (lit "sessions")
    <> rUsers . (lit "users")
    <> lit "songs" . song
    <> lit "albums" . album
    <> lit "artists" . artist
    )
    where
      song   =  rSongs
      album  =  rAlbums
             <> rAlbumInfo     </> albumId
             <> rAlbumArt      </> albumId  </> lit "art"
             <> rAlbumArtThumb </> albumId  </> lit "thumbnail"
             <> rAlbumSongs    </> albumId  </> lit "songs"
             <> rAlbumM3U      </> albumId  </> lit "m3u"
      artist =  rArtists
             <> rArtistInfo    </> artistId
             <> rArtistAlbums  </> artistId </> lit "albums"

artistId :: Router ArtistId
artistId =
    xmaph ArtistId (Just . unArtistId) int

albumId :: Router AlbumId
albumId =
    xmaph AlbumId (Just . unAlbumId) int

songId :: Router SongId
songId =
    xmaph SongId (Just . unSongId) int



