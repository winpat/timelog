module Storage.Serialize (load, save) where

import Types

filename = "labda-time.dat"

load :: IO Log
load = do 
  contents <- readFile filename
  return $ read contents


save :: Log -> IO ()
save ls = do
  writeFile filename $ show ls
