module Main where

import Data.Maybe
import Control.Monad
import Control.Applicative

import qualified Network.HTTP as HTTP
import qualified Network.URI as URI

import Text.XML.HaXml
import Text.XML.HaXml.Posn

main = do
  players <- httpGetPlayer
  putStrLn $ show players
  return ()

handleError :: Either String a -> a
handleError result = case result of
  Left e -> error e
  Right content -> content

unique :: [a] -> Either String a
unique (x:[]) = Right x
unique _ = Left "Error parsing"

httpGet :: String -> IO String
httpGet location = liftM handleError $ execute location
  where
    execute :: String -> IO (Either String String)
    execute location = do
      response <- HTTP.simpleHTTP request
      case response of
        Left e -> return $ Left $ "Error connecting: " ++ show e
        Right r -> case HTTP.rspCode r of
          (2,_,_) -> return $ Right (HTTP.rspBody r)
          (3,_,_) -> case HTTP.findHeader HTTP.HdrLocation r of
            Nothing -> return $ Left $ "No redirection URL provided: " ++ show r
            Just location -> execute location
          _ -> return $ Left $ "Error GET: " ++ show r
      where
        uri = fromJust $ URI.parseURI location
        request = HTTP.Request uri HTTP.GET [] ""

httpGetXML :: String -> IO (Content Posn)
httpGetXML location = liftM (parse "(No Document)") (httpGet location)
  where
    parse :: String -> String -> Content Posn
    parse name = getContent . xmlParse name . stripUnicodeBOM
      where
        getContent :: Document Posn -> Content Posn
        getContent (Document _ _ e _) = CElem e noPos

        stripUnicodeBOM :: String -> String
        stripUnicodeBOM ('\xef':'\xbb':'\xbf':x) = x
        stripUnicodeBOM x = x

httpGetPlayer :: IO Player
httpGetPlayer = liftM (handleError . unique . parse) get
  where
    parse :: Content Posn -> [Player]
    parse xml = map parsePlayer $ tag "INIT_PLAYER" xml

    get :: IO (Content Posn)
    get = httpGetXML "http://service.canal-plus.com/video/rest/initPlayer"

{-}
httpGetMedias :: String -> IO Medias
httpGetMedias i = liftM (handleError . unique . parse) get
  where
    parse :: Content Posn -> Medias
    parse xml = map parseMedias $ tag "MEAS" $ xml

    get :: String -> IO (Content Posn)
    get i = httpGetXML "http://service.canal-plus.com/video/rest/getMEAs/cplus/" ++ i
-}

xmlGetText :: CFilter Posn -> (Content Posn -> String)
xmlGetText path content = parseOrThrowError "UNDEFINED" $ path /> txt $ content
  where
    parseOrThrowError :: String -> [Content Posn] -> String
    parseOrThrowError message [] = error message
    parseOrThrowError _ contents = parse contents

    parse :: [Content Posn] -> String
    parse = concatMap process
      where
        process :: Content Posn -> String
        process x = verbatim $ keep /> txt $ CElem (unescape (fakeElement x)) noPos

        fakeElement :: Content i -> Element i
        fakeElement x = Elem (N "fake") [] [x]

        unescape :: Element i -> Element i
        unescape = xmlUnEscape stdXmlEscaper

xmlGetTextWithPath :: [String] -> Content Posn -> String
xmlGetTextWithPath path = xmlGetText $ xmlCreateFilter path
  where
    xmlCreateFilter :: [String] -> CFilter i
    xmlCreateFilter = foldr step keep where step x ys = ys /> tag x

data Player = Player
  { playerThematiques :: [Thematique]
  } deriving (Show)

data Thematique = Thematique
  { thematiqueId :: String
  , thematiqueName :: String
  , thematiqueSelections :: [Selection]
  } deriving (Show)

data Selection = Selection
  { selectionId :: String
  , selectionName :: String
  } deriving (Show)

data Medias = Medias
  { mediaList :: [Media]
  } deriving (Show)

data Media = Media
  { mediaId :: String
  , mediaTitle :: String
  , mediaDescription :: String
  } deriving (Show)

parsePlayer :: Content Posn -> Player
parsePlayer xml = Player
  { playerThematiques = map parseThematique $ keep /> tag "THEMATIQUES" /> tag "THEMATIQUE" $ xml
  }

parseThematique :: Content Posn -> Thematique
parseThematique xml = Thematique
  { thematiqueId = xmlGetTextWithPath ["ID"] xml
  , thematiqueName = xmlGetTextWithPath ["NOM"] xml
  , thematiqueSelections = map parseSelection $ keep /> tag "SELECTIONS" /> tag "SELECTION" $ xml
  }

parseSelection :: Content Posn -> Selection
parseSelection xml = Selection
  { selectionId = xmlGetTextWithPath ["ID"] xml
  , selectionName = xmlGetTextWithPath ["NOM"] xml
  }

parseMedias :: Content Posn -> Medias
parseMedias xml = Medias
  { mediaList = map parseMedia $ keep /> tag "MEA" $ xml
  }

parseMedia :: Content Posn -> Media
parseMedia xml = Media
  { mediaId = xmlGetTextWithPath ["ID"] xml
  , mediaTitle = xmlGetTextWithPath ["INFOS", "TITRAGE", "TITRE"] xml
  , mediaDescription = xmlGetTextWithPath [ "INFOS", "DESCRIPTION" ] xml
  }