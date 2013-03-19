module Main where

import ITV.Document
import ITV.CPlus.Types as CPlus

main = do
  iTVDocumentPrintDebug $ CPlus.PlayerURL
  iTVDocumentPrintDebug $ CPlus.MediasURL "105"
  iTVDocumentPrintDebug $ CPlus.VideosURL "825660"
  return ()
