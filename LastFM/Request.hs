{-# LANGUAGE OverloadedStrings #-}

module LastFM.Request where

import qualified Data.ByteString.Lazy.Char8 as BL
import Network.HTTP
import Data.Maybe
import Network.URI
import Data.Aeson
import LastFM.JSON
import LastFM.Types

downloadURL :: String -> IO (Either String BL.ByteString)
downloadURL url =
    do resp <- simpleHTTP request
       case resp of
         Left x -> return $ Left ("Error connecting: " ++ show x)
         Right r -> 
             case rspCode r of
               (2,_,_) -> return $ Right (rspBody r)
               (3,_,_) -> -- A HTTP redirect
                 case findHeader HdrLocation r of
                   Nothing -> return $ Left (show r)
                   Just url -> downloadURL url
               _ -> return $ Left (show r)
    where request = Request {rqURI = uri,
                             rqMethod = GET,
                             rqHeaders = [],
                             rqBody = ""}
          uri = fromJust $ parseURI url

lastFMUrl = "http://ws.audioscrobbler.com/2.0/?method=album.getinfo&format=json"

buildAlbumUrl :: String -> String -> String -> String
buildAlbumUrl key art alb = lastFMUrl ++ "&api_key=" ++ key ++ "&artist=" ++ (urlEncode art) ++ "&album=" ++ (urlEncode alb)

main :: IO ()
main = do
        let f = buildAlbumUrl "bdb236f5de89510054e48b6058f84713" "HELLOWEEN" "KEEPER OF THE SEVEN KEYS PART 2"
        r <- downloadURL f
        case r of
            Left x     -> print x   
            Right resp -> do
                            let json = decode resp :: Maybe LastFMResponse
                            print json
