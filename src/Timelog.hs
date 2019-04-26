module Timelog where

import Parser (parseEntry)

type Timelog = [Entry]
type Filepath = String

readTimelog :: Filepath -> IO Timelog
readTimelog f = do
            c <- readFile f
            let l = lines c
                e = map parseEntry l
            return e
