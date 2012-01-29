{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Main (main) where

import Control.Monad           (msum)
import Happstack.Server        (port, toResponse ,simpleHTTP, nullConf, dir, notFound, seeOther, serveDirectory, Browsing(..))
import Web.Routes.Happstack    (implSite)
import Data.Acid
import Control.Concurrent (killThread, forkIO)

import Auth
import Types
import RouteHandlers


main :: IO ()
main = do
        users <- openLocalState (UserMap emptyMap)
        sessions <- openLocalState (SessionMap emptyMap)
        sdb <- openLocalState (sdbEmpty)
        httpThreadId <- forkIO $ simpleHTTP nullConf{ port = 80 } $ 
                                 msum [ dir "favicon.ico" $ notFound (toResponse ())
                                      , implSite "http://core.lan" "/api/" (site sdb users sessions)
                                      , serveDirectory DisableBrowsing ["index.html"] "./static"
                                      , seeOther "" (toResponse ())
                                      ]
        c <- getChar
        killThread httpThreadId
        closeAcidState sdb
        closeAcidState users
        closeAcidState sessions
        putStrLn "Server stopped."
