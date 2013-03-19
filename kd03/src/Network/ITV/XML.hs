module Network.ITV.XML where

import Data.Aviary.Birds

import Text.XML.HaXml
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Parse

iTVXMLParse :: String -> Either String (Content Posn)
iTVXMLParse = fmap root . parse . strip
  where
    parse :: String -> Either String (Document Posn)
    parse = xmlParse' "(No Document)"

    root :: Document Posn -> Content Posn
    root (Document _ _ e _) = CElem e noPos

    strip :: String -> String
    strip ('\xef':'\xbb':'\xbf':xs) = xs
    strip xs = xs

prop_iTVXMLParse_checkFailure :: Bool -> String -> Bool
prop_iTVXMLParse_checkFailure flag string = ((failure flag) . iTVXMLParse) string
  where
    failure :: Bool -> Either a b -> Bool
    failure f = either (const f) (const (not f))

prop_iTVXMLParse_checkIdentity :: String -> Bool
prop_iTVXMLParse_checkIdentity = starling (==) identity
  where
    identity :: String -> String
    identity = showContent . readContent

    showContent :: Either String (Content Posn) -> String
    showContent = either (const undefined) verbatim

    readContent :: String -> Either String (Content Posn)
    readContent = iTVXMLParse
