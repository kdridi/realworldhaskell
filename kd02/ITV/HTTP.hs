module ITV.HTTP where

import Data.Maybe

import Network.HTTP
import Network.URI

import ITV.XML

iTVHTTPGetXML :: String -> IO (ITVXML)
iTVHTTPGetXML location = do
  content <- iTVHTTPGet location
  return $ iTVXMLParse "(No Document)" content

iTVHTTPGet :: String -> IO String
iTVHTTPGet location = do
  content <- fetchHTTPLocation location
  return $ case content of
    Left e -> error e
    Right x -> x
  where
    fetchHTTPLocation :: String -> IO (Either String String)
    fetchHTTPLocation location = do
      response <- simpleHTTP request
      case response of
        Left e -> return $ Left $ "Error connecting: " ++ show e
        Right r -> case rspCode r of
          (2,_,_) -> return $ Right (rspBody r)
          (3,_,_) -> case findHeader HdrLocation r of
            Nothing -> return $ Left $ "No redirection URL provided: " ++ show r
            Just location -> fetchHTTPLocation location
          _ -> return $ Left $ "Error GET: " ++ show r
      where
        uri = fromJust $ parseURI location
        request = Request {rqURI = uri, rqMethod = GET, rqHeaders = [], rqBody = ""}

