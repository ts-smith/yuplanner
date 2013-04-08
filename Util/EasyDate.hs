module Util.EasyDate where

import Prelude

--import Data.Time.Clock

import Data.Time.Calendar
--import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDate

import Data.Time.LocalTime
--import Data.Time.Format

--import System.Locale
--import System.Time

import Data.List.Split
 
--date :: IO (Integer,Int,Int) -- :: (year,month,day)
--date = getCurrentTime >>= return . toGregorian . utctDay

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
   deriving (Enum, Show, Bounded)

getDay :: IO Data.Time.Calendar.Day
getDay = fmap (localDay . zonedTimeToLocalTime) getZonedTime

getTime :: IO Data.Time.LocalTime.LocalTime
getTime = fmap zonedTimeToLocalTime getZonedTime

getPastMonday :: Day -> Int
getPastMonday day = monIndex - 1
   where (_,_,monIndex) = toWeekDate day

gpm :: Day -> Int
gpm = getPastMonday

nextFiveDays :: Day -> [Day]
nextFiveDays day = filter weekDay (sevendays day)

sevendays :: Day -> [Day]
sevendays day = map (\ x -> addDays x day) [0..6]

weekDay :: Day -> Bool
weekDay x = gpm x == 0 || (gpm x `mod` 5 /= 0 && gpm x `mod` 6 /= 0)

weeksFromNow :: Int -> Day -> Day
weeksFromNow n day = addDays ((fromIntegral n) *7) day

toString :: Int -> String
toString offset = show $ (toEnum offset :: Weekday)

niceFormat :: Int -> Day -> String
niceFormat offset day = toString offset ++ ", " ++ show day

--Month Functions

getMonth :: Day -> ([Day], Day)
getMonth givenDay = (map (fromGregorian year month) [1..monthLength], lastDay)
   where (year, month, _) = toGregorian givenDay
         monthLength = gregorianMonthLength year month
         lastDay = fromGregorian year month monthLength

frontPadding :: Day -> [(Day, Bool)]
frontPadding day = if offset /= 6 then 
      let startPad = negate (offset + 1)
          endPad = -1
          numPad = map (flip addDays day) $ map toInteger [startPad..endPad]
      in zip numPad $ repeat False
                                     else []
   where offset = gpm day

backPadding :: Day -> [(Day,Bool)]
backPadding day = if offset /= 5 then
      let startPad = 1
          endPad = 5 - offset
          numPad = map (flip addDays day) $ map toInteger [startPad..endPad]
      in zip numPad $ repeat False
                                    else []
   where offset = if gpm day == 6 then -1 else gpm day
         
createValidMonth :: Day -> [(Day,Bool)]
createValidMonth day = 
      frontPad ++ validDays ++ backPad
   where (month, lastDay) = getMonth day
         frontPad = frontPadding $ head month
         backPad = backPadding lastDay
         validDays = zip month $ repeat True

getWeeks :: [(Day,Bool)] -> [[(Day,Bool)]]
getWeeks paddedMonth = chunksOf 7 paddedMonth

makeWeeks :: Day -> [[(Day,Bool)]]
makeWeeks day = getWeeks $ createValidMonth day
