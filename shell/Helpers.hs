module Helpers where

import Data.Fixed
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime



mkUtc :: Integer -> Int -> Int -> Int -> Int -> Pico -> UTCTime
mkUtc y m d hh mm ss = UTCTime (fromGregorian y m d) (timeOfDayToTime $ TimeOfDay hh mm ss)

