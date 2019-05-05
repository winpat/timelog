module Types where

import Data.Time.Clock
import Data.Time.Format
import Data.Time.Clock.POSIX

data Entry = Entry {
    startTime :: Maybe UTCTime
  , endTime :: Maybe UTCTime
  , description :: String 
}  deriving (Eq, Read, Show)


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
        let format = "%H:%M"
        in formatTime defaultTimeLocale format . posixSecondsToUTCTime $ diffUTCTime end start