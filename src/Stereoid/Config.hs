module Stereoid.Config where

import Data.ConfigFile
import Control.Monad.Error
import Stereoid.Types (StereoidConfig(..))

getConfig :: String -> IO (Maybe StereoidConfig)
getConfig fp = do
        rv <- runErrorT $
            do
            cp <- join $ liftIO $ readfile emptyCP fp
            let x = cp
            sB <- get x "DEFAULT" "server_base"
            aP <- get x "DEFAULT" "api_path"
            rD <- get x "DEFAULT" "resource_directory"
            sD <- get x "DEFAULT" "state_directory"
            return (sB,aP,rD,sD)
        case rv of
            Left _ -> return Nothing
            Right (a,b,c,d) -> return $ Just (StereoidConfig a b c d)
