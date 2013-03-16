module CanalPlusTypes where

data Thematique = Thematique
  { thematiqueId :: String
  , thematiqueName :: String
  , thematiqueSelections :: [Selection]
  } deriving (Show)

data Selection = Selection
  { selectionId :: String
  , selectionName :: String
  } deriving (Show)
