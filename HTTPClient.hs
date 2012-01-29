{-# LANGUAGE OverloadedStrings #-}

module HTTPClient where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Network.HTTP
import Data.Maybe
import Network.URI
import Data.Aeson
import LastFM.JSON
import LastFM.Types

downloadFileWithMime :: String -> IO (Either String (Maybe B.ByteString,B.ByteString))
downloadFileWithMime url =
    do resp <- simpleHTTP request
       case resp of
         Left x -> return $ Left ("Error connecting: " ++ show x)
         Right r -> 
             case rspCode r of
               (2,_,_) -> 
                 case findHeader HdrContentType r of
                   Nothing   -> return $ Right (Nothing, rspBody r)
                   Just mime -> return $ Right (Just (C.pack mime), rspBody r)
               (3,_,_) -> -- A HTTP redirect
                 case findHeader HdrLocation r of
                   Nothing -> return $ Left (show r)
                   Just url -> downloadFileWithMime url
               _ -> return $ Left (show r)
    where request = Request {rqURI = uri,
                             rqMethod = GET,
                             rqHeaders = [],
                             rqBody = ""}
          uri = fromJust $ parseURI url

{-
main :: IO ()
main = do
        r <- downloadURL "http://www.popmatters.com/images/news_art/s/superchunk-majesty-shreddin.jpg"
        case r of
            Left x     -> print x   
            Right (Just mime, resp) -> do BL.writeFile "test.jpg" resp
                                          print mime
-}
