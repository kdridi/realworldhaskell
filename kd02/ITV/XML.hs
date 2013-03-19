module ITV.XML where

import Text.XML.HaXml
import Text.XML.HaXml.Posn

type ITVXML = Content Posn

iTVXMLParse :: String -> String -> ITVXML
iTVXMLParse name = getContent . xmlParse name . stripUnicodeBOM
  where
    getContent :: Document Posn -> ITVXML
    getContent (Document _ _ e _) = CElem e noPos

    stripUnicodeBOM :: String -> String
    stripUnicodeBOM ('\xef':'\xbb':'\xbf':x) = x
    stripUnicodeBOM x = x

iTVXMLGetText :: String -> ITVXML -> String
iTVXMLGetText name content = parseOrThrowError "UNDEFINED" $ keep /> tag name /> txt $ content
  where
    parseOrThrowError :: String -> [ITVXML] -> String
    parseOrThrowError message [] = error message
    parseOrThrowError _ contents = parse contents

    parse :: [ITVXML] -> String
    parse = concatMap process
      where
        process :: ITVXML -> String
        process x = verbatim $ keep /> txt $ CElem (unescape (fakeElement x)) noPos

        fakeElement :: Content i -> Element i
        fakeElement x = Elem (N "fake") [] [x]

        unescape :: Element i -> Element i
        unescape = xmlUnEscape stdXmlEscaper
