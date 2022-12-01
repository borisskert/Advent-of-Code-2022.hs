module Common.TextFile (readFileContents) where

import System.IO (IOMode (ReadMode), hGetContents, openFile)

readFileContents :: String -> IO String
readFileContents filename = do
  fileHandle <- openFile filename ReadMode
  hGetContents fileHandle
