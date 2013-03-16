{-- snippet all --}
module Main where

import CanalPlusTypes
import CanalPlusParser
import HTTPClient

import System.Environment

import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn

main = do
  doc <- httpGetXMLContent "http://service.canal-plus.com/video/rest/initPlayer"
  putStrLn $ show $ parseThematiques doc
  return ()

