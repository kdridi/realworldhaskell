module Main where

import Data.List
import Data.Maybe

import Control.Monad
import Control.Applicative

import qualified Network.HTTP as HTTP
import qualified Network.URI as URI

import Text.XML.HaXml
import Text.XML.HaXml.Posn

main = do
  putStrLn "################################################################################"
  thematics <- fetch $ ThematicsLocation :: IO Page
  putStrLn $ show thematics
  putStrLn "################################################################################"
  medias <- fetch $ MediasLocation "105" :: IO Page
  putStrLn $ show medias
  putStrLn "################################################################################"
  videos <- fetch $ VideosLocation "825660" :: IO Page
  putStrLn $ show videos
  putStrLn "################################################################################"
  return ()

data Thematic = Thematic
  { thematicId :: String
  , thematicName :: String
  , thematicSelections :: [Selection]
  } deriving (Show)

data Media = Media
  { mediaId :: String
  , mediaTitle :: String
  , mediaDescription :: String
  } deriving (Show)

data Video = Video
  { videoId :: String
  , videoName :: String
  , videoURLS :: [String]
  } deriving (Show)

data Selection = Selection
  { selectionId :: String
  , selectionName :: String
  } deriving (Show)

thematicParse :: Content Posn -> Thematic
thematicParse xml = Thematic
  { thematicId = string ["ID"] xml
  , thematicName = string ["NOM"] xml
  , thematicSelections = map selectionParse (each [ "SELECTIONS", "SELECTION" ] xml)
  }

selectionParse :: Content Posn -> Selection
selectionParse xml = Selection
  { selectionId = string ["ID"] xml
  , selectionName = string ["NOM"] xml
  }

mediaParse :: Content Posn -> Media
mediaParse xml = Media
  { mediaId = string ["ID"] xml
  , mediaTitle = string ["INFOS", "TITRAGE", "TITRE"] xml
  , mediaDescription = string [ "INFOS", "DESCRIPTION" ] xml
  }

videoParse :: Content Posn -> Video
videoParse xml = Video
  { videoId = string ["ID"] xml
  , videoName = string ["INFOS", "DESCRIPTION"] xml
  , videoURLS = map (verbatim . (keep /> txt)) (concatMap children (each [ "MEDIA", "VIDEOS" ] xml))
  }

data Page = Thematics [Thematic] | Medias [Media] | Videos [Video] deriving (Show)

data Location = ThematicsLocation | MediasLocation String | VideosLocation String

data ParserContext a = ParserContext [String] String (Content Posn -> a)

class LocationContext a where
  parserContext :: Location -> ParserContext a

instance LocationContext Page where
  parserContext (ThematicsLocation ) = ParserContext [           "initPlayer"] "INIT_PLAYER" $ \xml -> Thematics $ map thematicParse (each [ "THEMATIQUES", "THEMATIQUE" ] xml)
  parserContext ( MediasLocation i ) = ParserContext [  "getMEAs", "cplus", i]        "MEAS" $ \xml ->    Medias $ map    mediaParse (each [                       "MEA" ] xml)
  parserContext ( VideosLocation i ) = ParserContext ["getVideos", "cplus", i]      "VIDEOS" $ \xml ->    Videos $ map    videoParse (each [                     "VIDEO" ] xml)

class FetchableLocation a where
  fetch :: Location -> IO a

instance FetchableLocation Page where
  fetch = runParser . parserContext
    where
      runParser :: ParserContext a -> IO a
      runParser (ParserContext arguments name function) = fmap function (uncurry http get)
        where
          get :: (String, CFilter Posn)
          get = (location, path)

          location :: String
          location = intercalate "/" $ ["http://service.canal-plus.com/video/rest"] ++ arguments

          path :: CFilter Posn
          path = tag name

          http :: String -> CFilter Posn -> IO (Content Posn)
          http location elements = liftM (handleError .  unique .  elements . xml) (get location)
            where
              handleError :: Either String a -> a
              handleError result = case result of
                Left e -> error e
                Right content -> content

              get :: String -> IO String
              get location = liftM handleError $ execute location
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

              xml :: String -> Content Posn
              xml = getContent . xmlParse "(No Document)" . stripUnicodeBOM
                where
                  getContent :: Document Posn -> Content Posn
                  getContent (Document _ _ e _) = CElem e noPos

                  stripUnicodeBOM :: String -> String
                  stripUnicodeBOM ('\xef':'\xbb':'\xbf':x) = x
                  stripUnicodeBOM x = x

              unique :: [a] -> Either String a
              unique (x:[]) = Right x
              unique _ = Left "Error parsing"

each :: [String] -> CFilter i
each = foldl step keep where step ys x = ys /> tag x

string :: [String] -> Content Posn -> String
string = getText . each
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
