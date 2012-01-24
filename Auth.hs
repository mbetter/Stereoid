{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module Auth where

import Database.HDBC
import Text.JSON
import Happstack.Server        (port      , Response     , ServerPartT, ok           , toResponse
                               ,simpleHTTP, nullConf     , seeOther   , dir          , notFound
                               ,seeOther  , asContentType, serveFile  , ToMessage(..)                                                    )
import Happstack.Server.FileServe.BuildingBlocks (strictByteStringResponse, serveFileUsing)
import Happstack.Server.Modified (filePathSendAllowRange)
import Web.Routes.Boomerang    
import Web.Routes              ( PathInfo(..), RouteT    , showURL , runRouteT
                               , Site(..)    , setDefault, mkSitePI           )
import Data.Acid
import Data.Acid.Advanced
import Control.Monad.State     
import Control.Monad.Reader   
import Control.Applicative     ( (<$>) )
import Data.SafeCopy
import Safe
import Types
import Data.Typeable
import System.Random
import Control.Monad.Trans     (MonadIO, liftIO)
import qualified Happstack.Crypto.Base64 as B64
import Happstack.Crypto.SHA1
import Data.Time.Clock.POSIX
import Data.Char
import qualified Data.Map as Map

data SessionData = SessionData { userId :: StereoidId, sessionExpires :: Timestamp } | RememberData { rUserId :: StereoidId }
     deriving (Typeable)

$(deriveSafeCopy 0 'base ''SessionData)

data SessionMap = SessionMap !(Map.Map SessionToken SessionData)
     deriving (Typeable)

$(deriveSafeCopy 0 'base ''SessionMap)

data UserData = UserData { passHash :: String
                         } 
     deriving (Typeable)

$(deriveSafeCopy 0 'base ''UserData)

data UserMap = UserMap !(Map.Map StereoidId UserData)
     deriving (Typeable)
$(deriveSafeCopy 0 'base ''UserMap)

insertSession :: SessionToken -> SessionData -> Update SessionMap ()
insertSession key value
    = do SessionMap m <- get
         put (SessionMap (Map.insert key value m))

deleteSession :: SessionToken -> Update SessionMap ()
deleteSession key
    = do SessionMap m <- get
         put (SessionMap (Map.delete key m))

lookupSession :: SessionToken -> Query SessionMap (Maybe SessionData)
lookupSession key
    = do SessionMap m <- ask
         return (Map.lookup key m)

$(makeAcidic ''SessionMap ['insertSession, 'lookupSession, 'deleteSession])

insertUser :: StereoidId -> UserData -> Update UserMap ()
insertUser key value
    = do UserMap m <- get
         put (UserMap (Map.insert key value m))

lookupUser :: StereoidId -> Query UserMap (Maybe UserData)
lookupUser key
    = do UserMap m <- ask
         return (Map.lookup key m)

userExists :: StereoidId -> Query UserMap Bool
userExists key
    = do UserMap m <- ask
         return (Map.member key m)

$(makeAcidic ''UserMap ['insertUser, 'lookupUser, 'userExists])

newUser :: (Monad m, MonadIO m) => AcidState UserMap -> StereoidId -> String -> m (Either String StereoidId)
newUser acid username p 
    = do exists <- (query' acid (UserExists username)) 
         if exists then
             return (Left "Invalid Username")
         else do
             let ud = UserData { passHash = sha1 p } 
             update' acid (InsertUser username ud) 
             return (Right username)

authUser :: (Monad m, MonadIO m) => AcidState UserMap -> StereoidId -> String -> String -> m Bool
authUser acid username hash salt
    = do ue <- query' acid (LookupUser username) 
         case ue of
            Nothing -> return False
            Just UserData { passHash = ph } ->
                do let h2 = sha1 (ph ++ salt) 
                   if (h2 == hash) then
                        return True
                   else
                        return False
         
    

removeSession :: (Monad m, MonadIO m) => AcidState SessionMap -> SessionToken -> m ()
removeSession acid token
    = update' acid (DeleteSession token)
    
newSession :: (Monad m, MonadIO m) => AcidState SessionMap -> Integer -> StereoidId -> m SessionToken
newSession acid min uid
    = do now <- liftIO $ getPOSIXTime
         let expires = (floor now) + (min * 60)
         let ud = SessionData { userId = uid, sessionExpires = expires }
         token <- liftIO $ randomToken 48
         update' acid (InsertSession token ud)
         return token
    
newRememberMe :: (Monad m, MonadIO m) => AcidState SessionMap -> StereoidId -> m SessionToken
newRememberMe acid uid = do
         let ud = RememberData { rUserId = uid }
         token <- liftIO $ randomToken 64
         update' acid (InsertSession token ud)
         return token
    
checkRenewRememberMe :: (Monad m, MonadIO m) => AcidState SessionMap -> SessionToken -> StereoidId -> m (Maybe SessionToken)
checkRenewRememberMe acid token user = do
    oldT <- query' acid (LookupSession token)
    case oldT of
        Nothing                              -> return Nothing
        Just rd@RememberData {rUserId = uid} -> if uid == user then do
                                                    update' acid (DeleteSession token)
                                                    newtoken <- liftIO $ randomToken 64
                                                    update' acid (InsertSession newtoken rd)
                                                    return $ Just newtoken
                                                else return Nothing
        _                                    -> return Nothing
                                                
checkExtendSession :: (Monad m, MonadIO m) => 
                 AcidState SessionMap -> Integer -> SessionToken -> m (Maybe StereoidId)
checkExtendSession acid min token
    = do now <- liftIO $ getPOSIXTime
         oldT <- query' acid (LookupSession token)
         case oldT of
            Nothing -> do update' acid (DeleteSession token)
                          return Nothing
            Just SessionData { userId = uid, sessionExpires = exptime } ->
                 do if ((floor now) > exptime) then
                        return Nothing
                    else
                         do let expires = (floor now) + (min * 60)
                            let ud = SessionData { userId = uid, sessionExpires = expires }
                            update' acid (InsertSession token ud)
                            return (Just uid)
            _       -> return Nothing
          

extendSession :: (Monad m, MonadIO m) => 
                 AcidState SessionMap -> Integer -> SessionToken -> m (Maybe SessionToken)
extendSession acid min token
    = do now <- liftIO $ getPOSIXTime
         oldT <- query' acid (LookupSession token)
         case oldT of
            Nothing -> return Nothing
            Just SessionData { userId = uid } ->
                 do let expires = (floor now) + (min * 60)
                    let ud = SessionData { userId = uid, sessionExpires = expires }
                    update' acid (InsertSession token ud)
                    return (Just token)
            _       -> return Nothing
          

randomToken :: Int -> IO SessionToken
randomToken n = do
        rG <- newStdGen
        return $ take n $ B64.encode $ randoms rG
             
