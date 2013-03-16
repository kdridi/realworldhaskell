module CPlusTypes where

import Data.List

data CPlusURL = CPlusURLThematiques
	| CPlusURLMedia String

data CPlusURLDocument = Thematique
	{ thematiqueId :: String
	, thematiqueName :: String
	, thematiqueSelections :: [Selection]
	} | Media
	{ mediaId :: String
	, mediaType :: String
	, mediaInfos :: [Info]
	} deriving (Show)

data Info = Info
	{ infoDescription :: String
	, infoViewCount :: Int
	, infoScores :: [Score]
	} deriving (Show)

data Score = Score
	{ scoreAuthorized :: Bool
	, scoreMean :: Double
	, scoreCount :: Int
	} deriving (Show)

data Selection = Selection
	{ selectionId :: String
	, selectionName :: String
	} deriving (Show)

instance Show CPlusURL where
	show url =
		case url of
			CPlusURLThematiques -> build $ "initPlayer":[]
			(CPlusURLMedia i) -> build $ "getMEAs":"cplus":[] ++ [i]
		where
			build args = intercalate "/" $ "http://service.canal-plus.com":"video":"rest":[] ++ args