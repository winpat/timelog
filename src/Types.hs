module Types where

import Data.Time.Clock
import Data.Time.Format
import Data.Time.Clock.POSIX

data Entry = Entry {
    startTime :: Maybe UTCTime
  , endTime :: Maybe UTCTime
  , description :: String
}  deriving (Eq, Read, Show)

-- TODO: Does deriving Eq do the same?
-- instance Eq Entry where
--   (Entry s1 e1 d1) == (Entry s2 e2 d2) = s1 == s2 && e1 == e2 && d1 == d2

instance Ord Entry where
  (Entry s1 _ _) `compare` (Entry s2 _ _) = s1 `compare` s2

type Log = [Entry]


renderDate :: Maybe UTCTime -> String
renderDate Nothing     = ""
renderDate (Just date) =
        let format = "%d/%b"
        in formatTime defaultTimeLocale format (utctDay date)

renderDateDiff :: Maybe UTCTime -> Maybe UTCTime -> String
renderDateDiff Nothing _ = ""
renderDateDiff _ Nothing = ""
renderDateDiff (Just start) (Just end) =
        let format = "%H:%M:%S"
        in formatTime defaultTimeLocale format . posixSecondsToUTCTime $ diffUTCTime end start

renderTime :: UTCTime -> String
renderTime date =
        let format = "%H:%M:%S"
        in formatTime defaultTimeLocale format date
