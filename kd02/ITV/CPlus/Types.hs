module ITV.CPlus.Types where

import Data.List

import Control.Monad(liftM)

import ITV.Document
import ITV.XML

import Text.XML.HaXml.Combinators

instance Document CPlusURL where
  iTVDocumentToString url = case url of
    PlayerURL -> build $ "initPlayer":[]
    MediasURL mediaId -> build $ "getMEAs":"cplus":[] ++ [mediaId]
    VideosURL mediaId -> build $ "getVideos":"cplus":[] ++ [mediaId]
    where
      build args = intercalate "/" $ "http://service.canal-plus.com":"video":"rest":[] ++ args

instance Show CPlusURL where
  show = iTVDocumentToString

data CPlusURL = PlayerURL | MediasURL String | VideosURL String

data CPlus = Player
  { playerThematiques :: [Thematique]
  } deriving (Show)

data Thematique = Thematique
  { thematiqueId :: String
  , thematiqueName :: String
  , thematiqueSelections :: [Selection]
  } deriving (Show)

data Selection = Selection
  { selectionId :: String
  , selectionName :: String
  } deriving (Show)

iTVDocumentParse :: CPlusURL -> IO CPlus
iTVDocumentParse url = liftM parse $ iTVDocumentToXML url
  where
    parse = case url of
      PlayerURL -> parsePlayer
      _ -> undefined

    parsePlayer :: ITVXML -> CPlus
    parsePlayer content = Player $ map parse $ path content
      where
        path :: ITVXML -> [ITVXML]
        path = tag "INIT_PLAYER" /> tag "THEMATIQUES" /> tag "THEMATIQUE"

        parse :: ITVXML -> Thematique
        parse content = Thematique
          { thematiqueId = iTVXMLGetText "ID" content 
          , thematiqueName = iTVXMLGetText "NOM" content 
          , thematiqueSelections = parseSelections content
          }

        parseSelections :: ITVXML -> [Selection]
        parseSelections content =
          map parse $ path content
          where
            path = keep /> tag "SELECTIONS" /> tag "SELECTION"
            parse content = Selection
              { selectionId = iTVXMLGetText "ID" content 
              , selectionName = iTVXMLGetText "NOM" content 
              }


