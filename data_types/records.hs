module Records where

import Data.Time.Clock
import Data.Time.Format

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving Show

data LogEntry = LogEntry
    { timestamp :: UTCTime
    , logLevel :: LogLevel
    , message :: String
    } deriving Show

logLevelToString :: LogLevel -> String
logLevelToString = show

logEntryToString :: LogEntry -> String
logEntryToString logEntry = timeToString (timestamp logEntry) ++ ": " ++
                            logLevelToString (logLevel logEntry) ++ ": " ++
                            message logEntry


data Person = Person
    { firstName :: String
    , lastName :: String
    , age :: Int
    } deriving Show

updateLastName :: Person -> Person -> Person
updateLastName prsn1 prsn2 = prsn2 { lastName = lastName prsn1 }

abbrFirstName :: Person -> Person
abbrFirstName prsn =
  if length (firstName prsn) <= 2 then prsn else
    prsn { firstName = head (firstName prsn) : "." }
