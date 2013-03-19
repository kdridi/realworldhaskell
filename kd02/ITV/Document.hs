module ITV.Document where

import ITV.HTTP
import ITV.XML

import Text.XML.HaXml.Verbatim

class Document u where
  iTVDocumentToString :: u -> String

iTVDocumentToXML :: Document a => a -> IO ITVXML
iTVDocumentToXML = iTVHTTPGetXML . iTVDocumentToString

iTVDocumentPrintDebug :: Document a => a -> IO ()
iTVDocumentPrintDebug url = do
  putStrLn $ "################################################################################"
  putStrLn $ "##### XML Content for: " ++ iTVDocumentToString url
  putStrLn $ "################################################################################"
  content <- iTVDocumentToXML url
  putStrLn $ verbatim content
  putStrLn $ "################################################################################"
  return ()
