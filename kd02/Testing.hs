module Main where

import Data.List
import Data.Maybe

import Control.Monad
import Control.Applicative

import qualified Network.HTTP as HTTP
import qualified Network.URI as URI

import Text.XML.HaXml
import Text.XML.HaXml.Posn

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

newtype Parser a = Parser {
  parserExecute :: Content Posn -> a
}

instance Functor Parser where
  fmap f p = Parser (f . parserExecute p)

createFilter :: [String] -> CFilter i
createFilter = foldr step keep where step x ys = ys /> tag x

executeParser :: Parser b -> [String] -> Content Posn -> [b]
executeParser p path xml = map ( parserExecute p ) ( createFilter path xml )

stringParser :: [String] -> Parser String
stringParser = Parser . getText . createFilter
  where
    getText :: CFilter Posn -> (Content Posn -> String)
    getText path content = parseOrThrowError "UNDEFINED" $ path /> txt $ content
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

parseString :: [String] -> Content Posn -> String
parseString = parserExecute . stringParser

data Page a = Page
  { pageParser :: Parser a
  , pageArgs :: [String]
  , pageTag :: String
  }


--  fmap :: (a -> b) -> f a -> f b
--  (<$) :: a -> f b -> f a


-- "http://service.canal-plus.com/video/rest/"

playerPage :: Page Player
playerPage = Page playerParser ["initPlayer"] "INIT_PLAYER"

mediasPage :: String -> Page Medias
mediasPage i = Page mediasParser ["getMEAs", "cplus", i] "MEAS"


parsePage :: Page a -> IO [a]
parsePage page = do
  content <- get page
  return $ map (parser page) (match page content)
  where
    parser :: Page a -> Content Posn -> a
    parser page = (parserExecute . pageParser) page

    match :: Page a -> Content Posn -> [Content Posn]
    match page = tag (pageTag page)

    --get :: IO (Content Posn)
    get page = httpGetXML (url page)

    --url :: String
    url page = intercalate "/" $ ["http://service.canal-plus.com/video/rest"] ++ (pageArgs page)






data Player = Player
  { playerThematiques :: [Thematique]
  } deriving (Show)

playerParser :: Parser Player
playerParser = Parser parse
  where
    parse xml = Player
      { playerThematiques = executeParser thematiqueParser [ "THEMATIQUES", "THEMATIQUE" ] xml
      }

data Thematique = Thematique
  { thematiqueId :: String
  , thematiqueName :: String
  , thematiqueSelections :: [Selection]
  } deriving (Show)

thematiqueParser :: Parser Thematique
thematiqueParser = Parser parse
  where
    parse xml = Thematique
      { thematiqueId = parseString ["ID"] xml
      , thematiqueName = parseString ["NOM"] xml
      , thematiqueSelections = executeParser selectionParser [ "SELECTIONS", "SELECTION" ] xml
      }

data Selection = Selection
  { selectionId :: String
  , selectionName :: String
  } deriving (Show)

selectionParser :: Parser Selection
selectionParser = Parser parse
  where
    parse xml = Selection
      { selectionId = parseString ["ID"] xml
      , selectionName = parseString ["NOM"] xml
      }

data Medias = Medias
  { mediaList :: [Media]
  } deriving (Show)

mediasParser :: Parser Medias
mediasParser = Parser parse
  where
    parse xml = Medias
      { mediaList = executeParser mediaParser ["MEA"] xml
      }

data Media = Media
  { mediaId :: String
  , mediaTitle :: String
  , mediaDescription :: String
  } deriving (Show)

mediaParser :: Parser Media
mediaParser = Parser parse
  where
    parse xml = Media
      { mediaId = parseString ["ID"] xml
      , mediaTitle = parseString ["INFOS", "TITRAGE", "TITRE"] xml
      , mediaDescription = parseString [ "INFOS", "DESCRIPTION" ] xml
      }