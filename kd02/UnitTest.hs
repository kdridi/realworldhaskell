{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module Main where

import Control.Monad

import Data.Aviary.Birds
import Data.Char
import Data.Function
import Data.List

import Test.QuickCheck

import Text.XML.HaXml
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Verbatim

content :: String -> Either String (Content Posn)
content = fmap root . parse . strip
  where
    parse :: String -> Either String (Document Posn)
    parse = xmlParse' "(No Document)"

    root :: Document Posn -> Content Posn
    root (Document _ _ e _) = CElem e noPos

    strip :: String -> String
    strip ('\xef':'\xbb':'\xbf':x) = x
    strip x = x

checkContent :: Bool -> [String] -> IO [()]
checkContent flag = flip forM $ quickCheck . check flag
  where
    check :: Bool -> String -> Bool
    check = flip starling Main.content . test

    test :: Bool -> String -> Either String (Content Posn) -> Bool
    test always _ (Left _) = always
    test _ string (Right x) = string == verbatim x

fakeXML a b = "<" ++ a ++ ">" ++ b ++ "</" ++ a ++ ">"
fakeXMLSimple name i = fakeXML name ((map toLower name) ++ "_" ++ i)
fakeXMLId = fakeXMLSimple "ID"
fakeXMLName = fakeXMLSimple "NOM"
fakeXMLThematic i = fakeXML "THEMATIQUE" ((fakeXMLId i) ++ (fakeXMLName i))
fakeXMLThematics n = fakeXML "THEMATIQUES" $ intercalate "" $ map fakeXMLThematic $ map show $ take n [0..]
fakeXMLPlayer n = fakeXML "INIT_PLAYER" $ fakeXMLThematics n

assertFailure :: Bool -> Either a b -> Bool
assertFailure flag = either (const flag) (const (not flag))

assertContentFailure :: Bool -> String -> IO ()
assertContentFailure flag string = quickCheck (((assertFailure flag) . Main.content) string)

assertContentIsSuccessfullyParsed :: String -> IO ()
assertContentIsSuccessfullyParsed string = assertContentFailure False string >>
  quickCheck ( checkIdentity string )
  where
    checkIdentity :: String -> Bool
    checkIdentity = starling (==) identity

    identity :: String -> String
    identity = showContent . readContent

    showContent :: Either String (Content Posn) -> String
    showContent = either (const undefined) verbatim

    readContent :: String -> Either String (Content Posn)
    readContent = Main.content

--------------------------------------------------------------------------
-- main

main = do

  assertContentFailure True ""
  assertContentFailure True "not xml"
  assertContentFailure True "<a>last brace missing"
  assertContentFailure True "first brace missing</a>"

  forM (map fakeXMLPlayer $ take 10 [1..]) $ assertContentIsSuccessfullyParsed

  --quickCheck $ prop_content_parsed

--------------------------------------------------------------------------
-- the end.