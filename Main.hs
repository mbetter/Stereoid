{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Main (main) where

import Control.Monad           (msum)
import Happstack.Server        (port, toResponse ,simpleHTTP, nullConf, dir, notFound, seeOther, serveDirectory, Browsing(..))
import Web.Routes.Happstack    (implSite)
import Database.HDBC
import Database.HDBC.MySQL
import RouteHandlers
import Auth
import Data.Acid
import Data.Acid.Advanced
import Persistence
import Persistence.Types
import qualified Data.Map as Map
import Control.Concurrent (killThread, forkIO)


main :: IO ()
main = do
        users <- openLocalState (UserMap Map.empty)
        sessions <- openLocalState (SessionMap Map.empty)
        sdb <- openLocalState (sdbEmpty)
        httpThreadId <- forkIO $ simpleHTTP nullConf{ port = 80 } $ 
                                 msum [ dir "favicon.ico" $ notFound (toResponse ())
                                      , implSite "http://core.lan" "/api/" (site sdb users sessions)
                                      , serveDirectory DisableBrowsing ["index.html"] "./static"
                                      , seeOther "" (toResponse ())
                                      ]
        c <- getChar
        closeAcidState sdb
        closeAcidState users
        closeAcidState sessions
        putStrLn "Server stopped."
