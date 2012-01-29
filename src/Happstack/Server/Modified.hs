module Happstack.Server.Modified where

import qualified Data.ByteString.Char8 as C
import Control.Monad.Trans     (MonadIO, liftIO)
import Data.List.Split         (splitOn)
import Happstack.Server        (Response,ok)
import System.IO (IOMode(ReadMode),withBinaryFile, hFileSize)
import System.Directory (getModificationTime)
import System.Time (toUTCTime)
import Happstack.Server.Monads (askRq, ServerMonad, FilterMonad)
import Happstack.Server.Internal.Types (getHeader, setHeader, setRsCode)
import Happstack.Server.FileServe.BuildingBlocks (sendFileResponse)

filePathSendAllowRange :: (ServerMonad m, MonadIO m)
                 => String   -- ^ content-type string
                 -> FilePath -- ^ path to file on disk
                 -> m Response
filePathSendAllowRange contentType fp =
    do 
       rq <- askRq
       let b = getHeader "Range" rq
       count   <- liftIO $ withBinaryFile fp ReadMode hFileSize -- garbage collection should close this
       let (start,bytes) = case b of
                                Nothing -> (0,count)
                                Just x -> (stN,byN)
                                           where upX = C.unpack x
                                                 (fstX,sndX) = span (/='-') upX
                                                 stN = (read (tail (dropWhile (/='=') fstX))::Integer)
                                                 byN = case (tail sndX) of
                                                        [] -> count - stN
                                                        y:ys -> (read ys::Integer)
       modtime <- liftIO $ getModificationTime fp
       case b of
            Nothing -> return $ setHeader "Accept-Ranges" "bytes" $ sendFileResponse contentType fp (Just (toUTCTime modtime, rq)) 0 count
            Just _  -> setRsCode 206 $ setHeader "Content-Range" range $ setHeader "Accept-Ranges" "bytes" $ sendFileResponse contentType fp (Just (toUTCTime modtime, rq)) start bytes
                       where range = "bytes " ++ (show start) ++ "-" ++ (show (start + bytes)) ++ "/" ++ (show count) 

