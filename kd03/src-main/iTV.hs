{--------------------------------------------------------------------------------
Copyright (c) Daan Leijen 2003
wxWindows License.

Demonstrates: 
- many different kind of controls
- message logging.
--------------------------------------------------------------------------------}
module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore hiding (Event)

main :: IO ()
main = start gridGUI

createTabButtons :: Window a -> IO () -> IO TabPage
createTabButtons w quit = do
  c     <- panel w []
  bOk   <- button c [text :=   "Ok", on command := logMessage "ok button pressed"]
  bQuit <- button c [text := "Quit", on command := quit]
  return $ tab "buttons" $ container c $ margin 10 $ floatCentre $ row 5
    [ widget bOk
    , widget bQuit
    ]

createTabRadioBox :: Window a -> IO TabPage
createTabRadioBox w = do
  c               <- panel w []
  rbLeft          <- radioBox c Vertical ["first", "second", "third"]
    [ text        := "radio box"
    , on select  ::= logSelect
    ]
  rbRight         <- radioBox c Horizontal ["first", "second", "third"]
    [ tooltip     := "radio group two"
    , on select  ::= logSelect
    ]
  bToggle         <- button c
    [ text        := "disable"
    , on command ::= onEnable rbLeft
    ]
  return $ tab "radio box" $  container c $ margin 10 $ column 5
    [ hstretch $ widget bToggle
    , row 0
      [ floatLeft $ widget rbLeft
      , floatRight $ widget rbRight
      ]
    ]

createTabChoice :: Window a -> IO TabPage
createTabChoice w = do
  c               <- panel w []
  cUChoices       <- choice c
    [ tooltip     := "unsorted choices"
    , on select  ::= logSelect
    , sorted      := False
    , items       := ["mies","noot","aap"]
    ]
  cSChoices       <- choice c
    [tooltip      := "sorted choices"
    , on select  ::= logSelect
    , sorted      := True
    , items       := ["mies","noot","aap"]
    ]
  bToggle         <- button c
    [ text        := "disable"
    , on command ::= onEnable cUChoices
    ]
  return $ tab "choice" $ container c $ margin 10 $ column 5
    [ hstretch $ widget bToggle
    , row 0
      [ floatLeft $ widget cUChoices
      , floatRight $ row 5
        [ label "sorted: "
        , widget cSChoices
        ]
      ]
    ]

createTabListBox :: Window a -> IO TabPage
createTabListBox w = do
  c               <- panel w []
  slbLeft         <- singleListBox c
    [ items       := ["mies","noot","aap"]
    , tooltip     := "unsorted single-selection listbox"
    , on select  ::= logSelect
    ]
  slbRight        <- singleListBox c 
    [ items       := ["mies","noot","aap"]
    , tooltip     := "sorted listbox"
    , on select  ::= logSelect
    , sorted      := True
    ]
  bToggle         <- checkBox c
    [ text        := "enable the listbox"
    , checked     := True
    , on command  := set slbLeft
      [ enabled   :~ not
      ]
    ]
  return $ tab "listbox" $ container c $ margin 10 $ column 5
    [ hstretch  $ dynamic $ widget bToggle
    , floatLeft $ row 0
      [ widget slbLeft
      , widget slbRight
      ]
    ]

createTabSlider :: Window a -> IO TabPage
createTabSlider nb = do
  p5               <- panel nb []
  s                 <- hslider p5 True {- show labels -} 1 100
    [ selection     := 50
    ]
  g                 <- hgauge p5 100
    [ selection     := 50
    ]
  set s
    [ on command    := do
      i             <- get s selection
      set g
        [ selection := i]
    ]
  return $ tab "slider" $ container p5 $ margin 10 $ column 5
    [ hfill $ widget s
    , hfill $ widget g
    , glue
    ]

createTabs :: Closeable w => Window (CControl (CNotebook a)) -> w -> IO Layout
createTabs nb f = do
  tabButtons <- createTabButtons nb (close f)
  tabRadioBox <- createTabRadioBox nb
  tabChoice <- createTabChoice nb
  tabListBox <- createTabListBox nb
  tabSlider <- createTabSlider nb
  return $ tabs nb
    [ tabButtons
    , tabRadioBox
    , tabChoice
    , tabListBox
    , tabSlider
    ]

gui :: IO ()
gui = do -- main gui elements: frame, panel, text control, and the notebook
  f       <- frame [text := "Controls"]
  p       <- panel f []
  nb      <- notebook p []
  textlog <- textCtrl p
    [ enabled := False
    , wrap := WrapNone
    ]
    
  -- use text control as logger
  textCtrlMakeLogActiveTarget textlog
  logMessage "logging enabled"              
  -- set f [on closing :~ \prev -> do logSetActiveTarget oldlog; logDelete log; prev]
  theTabs <- createTabs nb f

  set f
    [ layout := container p $ column 0
      [ theTabs
      , hfill $ minsize (sz 20 80) $ widget textlog
      ]
    , clientSize := sz 800 600
    ]
  return ()


onEnable :: (Textual t, Able a) => a -> t -> IO ()
onEnable w b = do
  set w [enabled :~ not]
  enable <- get w enabled
  set b
    [ text := (if enable then "disable" else "enable")
    ]

logSelect :: (Selection w, Items w [Char]) => w -> IO ()
logSelect w = do
  i <- get w selection
  s <- get w (item i)
  logMessage ("selected index: " ++ show i ++ ": " ++ s)




{--------------------------------------------------------------------------------
   Test Grid.
--------------------------------------------------------------------------------}

gridGUI :: IO ()
gridGUI = do
  f <- frame [text := "Grid test", visible := False] 
     
  -- use text control as logger
  textlog <- textCtrl f [wrap := WrapNone, enabled := False] 
  textCtrlMakeLogActiveTarget textlog
  logMessage "logging enabled"              

  -- grids
  g <- gridCtrl f []
  gridSetGridLineColour g (colorSystem Color3DFace)
  gridSetCellHighlightColour g black
  appendColumns g (head names)
  appendRows    g (map show [1..length (tail names)])
  mapM_ (setRow g) (zip [0..] (tail names))
  gridAutoSize g

  windowOnKeyDown g (onGridKeyDown g)
  set g [on gridEvent := onGrid]

  -- layout
  set f
    [ layout := column 5
      [ fill (dynamic (widget g))
      , hfill $ minsize (sz 20 80) $ widget textlog
      ]
    ]       
  focusOn g
  set f [visible := True]  -- reduce flicker at startup.
  return ()

  where
    onGridKeyDown g (EventKey k _ _) = case k of
      KeyReturn
        -> do
          logMessage "keyEnter"
          gridMoveNext g
      _ -> propagateEvent

    onGrid ev = case ev of
      GridCellChange r col _
        -> logMessage ("cell changed: " ++ show (r,col))
      _ -> propagateEvent

names :: [[String]]
names =
  [ [ "First Name"  , "Last Name"     ]
  , [         "Daan",         "Leijen"]
  , [        "Arjan", "van IJzendoorn"]
  , [      "Martijn",        "Schrage"]
  , [       "Andres",            "Loh"]
  ]

setRow :: Grid a -> (Int, [String]) -> IO ()
setRow g (r,vs) = mapM_
  (\(col,v) -> gridSetCellValue g r col v)
  (zip [0..] vs)

{--------------------------------------------------------------------------------
   Library?
--------------------------------------------------------------------------------}

gridCtrl :: Window a -> [Prop (Grid ())] -> IO (Grid ())
gridCtrl p props = feed2 props 0 $ initialWindow $ \i r xs flags ->
  gridCreate p i r flags >>= \g ->
  gridCreateGrid g 0 0 0 >>
  set g xs >>
  return g

gridEvent :: Event (Grid a) (EventGrid -> IO ())
gridEvent = newEvent "gridEvent" gridGetOnGridEvent gridOnGridEvent


gridMoveNext :: Grid a -> IO ()
gridMoveNext g = do
  r <- gridGetGridCursorRow g
  col <- gridGetGridCursorCol g
  rowCount <- gridGetNumberRows g
  colCount <- gridGetNumberCols g
  let (x,c) = if (r+1 >= rowCount)
                  then if (col+1 >= colCount)
                     then (0,0)
                     else (0,col+1)
                    else (r+1,col)
  gridSetGridCursor g x c
  gridMakeCellVisible g x c
  return ()


appendColumns :: Grid a -> [String] -> IO ()
appendColumns = append gridGetNumberCols gridAppendCols gridSetColLabelValue

appendRows :: Grid a -> [String] -> IO ()
appendRows = append gridGetNumberRows gridAppendRows gridSetRowLabelValue


append :: (Grid a -> IO Int) -> (Grid a -> Int -> Bool -> IO Bool) -> (Grid a -> Int -> String -> IO ()) -> Grid a -> [String] -> IO ()
append _ _ _ _ [] = return ()
append f1 f2 f3 g labels =
  f1 g >>= \n ->
  f2 g (length labels) True >>
  mapM_ (\(i,l) -> f3 g i l) (zip [n..] labels)

