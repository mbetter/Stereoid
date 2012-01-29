module ClearThumbs where

import Control.Exception (bracket)

import FileSystem
import Persistence

import Data.Acid
import Data.Acid.Advanced
import Types
import Jobs

main :: IO ()
main = bracket
    (openLocalState (sdbEmpty))
    (closeAcidState)
    (clearThumbnails)
