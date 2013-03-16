module HTTPClient where

import Data.Maybe

import Network.HTTP
import Network.URI

import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn

httpGetContent :: String -> IO String
httpGetContent url = do
  content <- fetch url
  return $ case content of
    Left e -> error e
    Right x -> x
  where
    fetch :: String -> IO (Either String String)
    fetch url = do
      res <- simpleHTTP request
      case res of
        Left e -> return $ Left $ "Error connecting: " ++ show e
        Right r -> case rspCode r of
          (2,_,_) -> return $ Right (rspBody r)
          (3,_,_) -> case findHeader HdrLocation r of
            Nothing -> return $ Left $ "No redirection URL provided: " ++ show r
            Just url -> fetch url
          _ -> return $ Left $ "Error GET: " ++ show r
      where
        uri = fromJust $ parseURI url
        request = Request {rqURI = uri, rqMethod = GET, rqHeaders = [], rqBody = ""}


-- httpGetXMLContent "http://service.canal-plus.com/video/rest/initPlayer"

--httpGetXMLContent :: String -> IO (Content Posn)
httpGetXMLContent url = do
  content <- httpGetContent url
  return $ parseContent "(No Document)" content
  where
  	parseContent name content = doc
		where
			parseResult = xmlParse name (stripUnicodeBOM content)
			doc = getContent parseResult

			getContent :: Document Posn -> Content Posn
			getContent (Document _ _ e _) = CElem e noPos

			stripUnicodeBOM :: String -> String
			stripUnicodeBOM ('\xef':'\xbb':'\xbf':x) = x
			stripUnicodeBOM x = x
