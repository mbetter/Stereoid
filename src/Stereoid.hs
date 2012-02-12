{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Main (main) where

import Control.Monad           (msum)
import Happstack.Server        (port, toResponse ,simpleHTTP, nullConf, dir, notFound, seeOther, serveDirectory, Browsing(..))
import Web.Routes.Happstack    (implSite)
import Data.Acid
import Control.Concurrent (killThread, forkIO)
import System (getArgs)

import Stereoid.Auth
import Stereoid.Types
import Stereoid.Config
import Stereoid.RouteHandlers
import Stereoid.Conversion

main :: IO ()
main = do
        config <- getConfig "/root/projects/Stereoid/stereoid.config" 
        case config of
            Nothing -> return ()
            Just (StereoidConfig host apidir resourcedir statedir) -> do
                users <- openLocalStateFrom (statedir ++ "Stereoid.Auth.UserMap") (UserMap emptyMap)
                sessions <- openLocalStateFrom (statedir ++ "Stereoid.Auth.SessionMap") (SessionMap emptyMap)
                sdb <- openLocalStateFrom (statedir ++ "Stereoid.Types.StereoidDb") (sdbEmpty)
                args <- getArgs
                doArgs args sdb
                httpThreadId <- forkIO $ simpleHTTP nullConf{ port = 80 } $ 
                                         msum [ dir "favicon.ico" $ notFound (toResponse ())
                                              , implSite host apidir (site sdb users sessions resourcedir statedir)
                                              , serveDirectory DisableBrowsing ["index.html"] (resourcedir ++ "static")
                                              , seeOther "" (toResponse ())
                                              ]
                c <- getChar
                killThread httpThreadId
                closeAcidState sdb
                closeAcidState users
                closeAcidState sessions
                putStrLn "Server stopped."
                where doArgs [] _                  = return ()
                      doArgs (x:_) sdb | x == "-c" = runConversion sdb
                      doArgs _ _                   = return ()
