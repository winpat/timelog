module Types where

import Data.Time.Clock
import Data.Time.Format

data Entry = Entry {
    startTime :: UTCTime
  , endTime :: UTCTime
  , description :: String 
}  deriving (Eq, Read, Show)


type Log = [Entry]
