{-# LANGUAGE OverloadedStrings #-}

module LastFM.Request where

import qualified Data.ByteString.Lazy.Char8 as BL
import Network.HTTP
import Data.Maybe
import Network.URI
import Data.Aeson
import LastFM.JSON
import LastFM.Types
import qualified Persistence.Types as P
import qualified Data.Text as T

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

lastFMUrl = "http://ws.audioscrobbler.com/2.0/?format=json"
-- lastFMUrl = "http://ws.audioscrobbler.com/2.0/?method=album.getinfo&format=json"

apiKey = "bdb236f5de89510054e48b6058f84713"

buildAlbumUrl :: String -> T.Text -> T.Text -> String
buildAlbumUrl key art alb = lastFMUrl ++ "&method=album.getinfo&api_key=" ++ key ++ "&artist=" ++ (urlEncode $ T.unpack $ T.toLower art) ++ "&album=" ++ (urlEncode $ T.unpack $ T.toLower alb)

{-
data Wiki = Wiki { wSummary :: T.Text
                 , wContent :: T.Text
                 } deriving (Show,Eq,Ord,Typeable)

data MetaData = MetaData { mdMbid :: Maybe T.Text
                         , mdTags :: [T.Text]
                         , mdWiki :: Maybe Wiki
                         } deriving (Show,Eq,Ord,Typeable)

mdEmpty = MetaData Nothing [] Nothing

data ArtAlt = FileArt B.ByteString B.ByteString |
              UrlArt B.ByteString deriving (Show,Eq,Ord,Typeable)

data ArtAltData = ArtAltData [ArtAlt] deriving (Show,Eq,Ord,Typeable)
data Album = Album { name :: T.Text
                   , artist :: Maybe T.Text
                   , mbid :: Maybe T.Text
                   , image :: Maybe [Image]
                   , listeners :: Maybe T.Text
                   , playcount :: Maybe T.Text
                   , toptags :: Maybe Tags
                   , wiki :: Maybe Wiki
                   } deriving (Show)
data Image = Image { text :: T.Text
                   , size :: T.Text
                   } deriving (Show)
data Tags = Tags { tags :: [Tag] } deriving (Show)
data Tag = Tag { tName :: T.Text
               , tUrl :: T.Text
               } deriving (Show)
data Wiki = Wiki { summary :: T.Text
                 , content :: T.Text
                 } deriving (Show)
                 -}
lastToPersistence :: Album -> (P.ArtAltData, P.MetaData)
lastToPersistence (Album _ _ m i _ _ t w)  = (aad i, md m t w)
                                             where aad is = P.ArtAltData $ map (\x -> (P.LastFMArt (size x) (text x))) is 
                                                   md mb ta wi = P.MetaData (fmap id mb) (map tName (tags ta)) (fmap f wi)
                                                   f wi = P.Wiki (summary wi) (content wi)

getAlbumInfo ::  T.Text -> T.Text -> IO (Maybe (P.ArtAltData, P.MetaData))
getAlbumInfo artist album = do
    let url = buildAlbumUrl apiKey artist album
    r <- downloadURL url
    case r of
        Left x     -> return Nothing
        Right resp -> do
                let lfmr = decode resp :: Maybe LastFMResponse
                case lfmr of
                    Just (LastFMResponse album) -> return $ Just (lastToPersistence album)
                    _                           -> return Nothing

{-
main :: IO ()
main = do
        let f = buildAlbumUrl "bdb236f5de89510054e48b6058f84713" "HELLOWEEN" "KEEPER OF THE SEVEN KEYS PART 2"
        r <- downloadURL f
        case r of
            Left x     -> print x   
            Right resp -> do
                            let json = decode resp :: Maybe LastFMResponse
                            print json
-}
