module CanalPlusParser where

import CanalPlusTypes

import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn

parseThematiques :: Content Posn -> [Thematique]
parseThematiques content =
	map parse $ path content
	where
		path = tag "INIT_PLAYER" /> tag "THEMATIQUES" /> tag "THEMATIQUE"
		parse content = Thematique
			{ thematiqueId = contentTextForTagName content "ID"
			, thematiqueName = contentTextForTagName content "NOM"
			, thematiqueSelections = parseSelections content
			}

parseSelections :: Content Posn -> [Selection]
parseSelections content =
	map parse $ path content
	where
		path = keep /> tag "SELECTIONS" /> tag "SELECTION"
		parse content = Selection
			{ selectionId = contentTextForTagName content "ID"
			, selectionName = contentTextForTagName content "NOM"
			}

contentTextForTagName :: Content Posn -> String -> String
contentTextForTagName content name =
	parseTextWithDefaultMessage "UNDEFINED" $ keep /> tag name /> txt $ content
	where
		parseTextWithDefaultMessage :: String -> [Content Posn] -> String
		parseTextWithDefaultMessage message [] = message
		parseTextWithDefaultMessage _ contents = parseText contents

		parseText :: [Content Posn] -> String
		parseText = 
		    concatMap processContent
		    where processContent :: Content Posn -> String
		          processContent x = verbatim $ keep /> txt $ CElem (unescape (fakeElementFromContent x)) noPos

		          fakeElementFromContent :: Content i -> Element i
		          fakeElementFromContent x = Elem (N "fake") [] [x]

		          unescape :: Element i -> Element i
		          unescape = xmlUnEscape stdXmlEscaper
