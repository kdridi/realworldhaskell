{-# OPTIONS_GHC -W #-}

module Main where

import CPlusTypes
import CPlusParser
import HTTPClient

import Text.XML.HaXml
import Text.XML.HaXml.Posn

main = do
  putStrLn "CPlusURLMedia \"105\""
  doc <- parse $ CPlusURLMedia "105"
  putStrLn $ show doc

  putStrLn "CPlusURLThematiques"
  doc <- parse $ CPlusURLThematiques
  putStrLn $ show doc

  return ()

parse :: CPlusURL -> IO [CPlusURLDocument]
parse url = do
	doc <- httpGetXMLContent $ show url
	return $ function url doc
	where
		function :: CPlusURL -> Content Posn -> [CPlusURLDocument]
		function url =
			case url of
				CPlusURLThematiques -> parseThematiques
				CPlusURLMedia _ -> parseMedias
