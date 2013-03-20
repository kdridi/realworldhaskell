{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module Main where

import Data.Char
import Data.List

import Network.ITV.XML

import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

main :: IO ()
main = defaultMain tests

fakeXML :: [Char] -> [Char] -> [Char]
fakeXML a b = if null b then ("<" ++ a ++ "/>") else ("<" ++ a ++ ">" ++ b ++ "</" ++ a ++ ">")

fakeXMLNode :: [Char] -> [Char] -> [Char]
fakeXMLNode tagName tagIdent = fakeXML tagName ((map toLower tagName) ++ "_" ++ tagIdent )

fakeXMLNodeID :: [Char] -> [Char]
fakeXMLNodeID = fakeXMLNode "ID"

fakeXMLNodeNAME :: [Char] -> [Char]
fakeXMLNodeNAME = fakeXMLNode "NOM"

fakeXMLNodeTHEMATIC :: [Char] -> [Char]
fakeXMLNodeTHEMATIC tagIdent = fakeXML "THEMATIQUE" (( fakeXMLNodeID tagIdent ) ++ ( fakeXMLNodeNAME tagIdent ))

fakeXMLNodeTHEMATICS :: Int -> [Char]
fakeXMLNodeTHEMATICS tagCount = fakeXML "THEMATIQUES" $ intercalate "" $ map fakeXMLNodeTHEMATIC $ map show $ take tagCount [0..]

fakeXMLNodePLAYER :: Int -> [Char]
fakeXMLNodePLAYER tagCount = fakeXML "INIT_PLAYER" $ fakeXMLNodeTHEMATICS tagCount

tests :: [TF.Test]
tests =
  [ testGroup "checkFailure: True"
    [ testProperty "TEXT: \"\""                     $ prop_iTVXMLParse_checkFailure True  $ ""
    , testProperty "TEXT: not xml"                  $ prop_iTVXMLParse_checkFailure True  $ "not xml"
    , testProperty "TEXT: <a>last brace missing"    $ prop_iTVXMLParse_checkFailure True  $ "<a>last brace missing"
    , testProperty "TEXT: first brace missing</a>"  $ prop_iTVXMLParse_checkFailure True  $ "first brace missing</a>"
    ]
  , testGroup "checkFailure: False"
    [ testProperty "fakeXMLNodePLAYER 0"            $ prop_iTVXMLParse_checkFailure False $ fakeXMLNodePLAYER 0
    , testProperty "fakeXMLNodePLAYER 1"            $ prop_iTVXMLParse_checkFailure False $ fakeXMLNodePLAYER 1
    , testProperty "fakeXMLNodePLAYER 5"            $ prop_iTVXMLParse_checkFailure False $ fakeXMLNodePLAYER 5
    ]
  , testGroup "checkIdentity"
    [ testProperty "fakeXMLNodePLAYER 0"            $ prop_iTVXMLParse_checkIdentity      $ fakeXMLNodePLAYER 0
    , testProperty "fakeXMLNodePLAYER 1"            $ prop_iTVXMLParse_checkIdentity      $ fakeXMLNodePLAYER 1
    , testProperty "fakeXMLNodePLAYER 5"            $ prop_iTVXMLParse_checkIdentity      $ fakeXMLNodePLAYER 5
    ]
  ]