module CPlusParser where

import CPlusTypes

import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn

parseThematiques :: Content Posn -> [CPlusURLDocument]
parseThematiques content =
	map parse $ path content
	where
		path = tag "INIT_PLAYER" /> tag "THEMATIQUES" /> tag "THEMATIQUE"
		parse content = Thematique
			{ thematiqueId = contentTextForTagName "ID" content 
			, thematiqueName = contentTextForTagName "NOM" content 
			, thematiqueSelections = parseSelections content
			}

parseMedias :: Content Posn -> [CPlusURLDocument]
parseMedias content =
	map parse $ path content
	where
		path = tag "MEAS" /> tag "MEA"
		parse content = Media
			{ mediaId = contentTextForTagName "ID" content 
			, mediaType = contentTextForTagName "TYPE" content 
			, mediaInfos = parseInfo "INFOS" content
			}

parseScore :: String -> Content Posn -> [Score]
parseScore name content =
	map parse $ path content
	where
		path = keep /> tag name
		parse content = Score
			{ scoreAuthorized = not ("NON" == contentTextForTagName "AUTORISER" content)
			, scoreMean = read $ contentTextForTagName "MOYENNE" content
			, scoreCount = read $ contentTextForTagName "NB_VOTES" content
			}

parseInfo :: String -> Content Posn -> [Info]
parseInfo name content =
	map parse $ path content
	where
		path = keep /> tag name
		parse content = Info
			{ infoDescription = contentTextForTagName "DESCRIPTION" content 
			, infoViewCount = read $ contentTextForTagName "NB_VUES" content 
			, infoScores = parseScore "NOTE" content
			}

parseSelections :: Content Posn -> [Selection]
parseSelections content =
	map parse $ path content
	where
		path = keep /> tag "SELECTIONS" /> tag "SELECTION"
		parse content = Selection
			{ selectionId = contentTextForTagName "ID" content 
			, selectionName = contentTextForTagName "NOM" content 
			}

contentTextForTagName :: String -> Content Posn -> String
contentTextForTagName name content =
	parseText $ keep /> tag name /> txt $ content
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
