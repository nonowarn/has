{-# OPTIONS_GHC -fglasgow-exts #-}

import qualified Data.Time as Time
import qualified Data.Fixed as Fixed
import qualified Data.List as List

import Data.Has

-- Define Entities

data Day = Day; type instance TypeOf Day = Integer
data Hour = Hour; type instance TypeOf Hour = Int
data Minute = Minute; type instance TypeOf Minute = Int
data Second = Second; type instance TypeOf Second = Fixed.Pico
data TimeZone = TimeZone; type instance TypeOf TimeZone = Time.TimeZone

-- Define Records
-- Note reusing entities here

type TimeOfDay = RowOf Hour :&: RowOf Minute :&: RowOf Second

type Time = RowOf Day :&: RowOf Hour :&: RowOf Minute :&: RowOf Second

type ZonedTime = RowOf TimeZone :&: RowOf Day
    :&: RowOf Hour :&: RowOf Minute :&: RowOf Second

-- But that was too verbose
-- You can write same type as follows
-- > type Time = RowOf Day :&: TimeOfDay
-- > type ZonedTime = RowOf TimeZone :&: Time

-- And you can write:

tomorrow :: (Has Day a) => a -> a
tomorrow = Day ^: succ

yesterday :: (Has Day a) => a -> a
yesterday = Day ^: pred

addHours :: (Has Day a, Has Hour a)
          => a -> Int -> a
addHours time hours =
    let (add_day, new_hour) = ((Hour ^. time) + hours) `divMod` 24
    in Day ^: (+ toInteger add_day) $ Hour ^= new_hour $ time

getTod :: (Has Hour a, Has Minute a, Has Second a)
       => a -> TimeOfDay
getTod a = rowOf (Hour ^. a)
         & rowOf (Minute ^. a)
         & rowOf (Second ^. a)

getUnixTime :: IO Integer
getUnixTime = fmap calc getUTCTime
  where
    calc time = ((Day ^. time) - epoch) * (24*60*60)
              + fromIntegral (todToSeconds time)

todToSeconds :: (Has Hour a, Has Minute a, Has Second a)
             => a -> Int
todToSeconds time = (Hour ^. time) * (60*60)
                  + (Minute ^. time) * 60
                  + round (Second ^. time)

-- Try
-- > fmap (showTime . tomorrow) getZonedTime
-- > fmap (showTime . tomorrow) getUTCTime
-- > fmap (showTime . (`addHours` 10))    getZonedTime
-- > fmap (showTime . (`addHours` (-10))) getUTCTime
-- > fmap (todToSeconds . getTod) getZonedTime
-- > fmap  todToSeconds           getZonedTime

-- Auxiliary Functions...
                
getUTCTime :: IO Time
getUTCTime = fmap fromUTCTime Time.getCurrentTime

getZonedTime :: IO ZonedTime
getZonedTime = fmap fromZonedTime Time.getZonedTime

epoch :: Integer
epoch = Time.toModifiedJulianDay $ Time.fromGregorian 1970 1 1

fromUTCTime :: Time.UTCTime -> Time
fromUTCTime utctime =
    let tod = Time.timeToTimeOfDay (Time.utctDayTime utctime)
    in rowOf (Time.toModifiedJulianDay (Time.utctDay utctime))
     & fromTOD tod

fromZonedTime :: Time.ZonedTime -> ZonedTime
fromZonedTime zonedtime =
    let localtime = Time.zonedTimeToLocalTime zonedtime
        day       = Time.toModifiedJulianDay $ Time.localDay localtime
        tod       = Time.localTimeOfDay localtime
    in rowOf (Time.zonedTimeZone zonedtime)
     & rowOf day
     & fromTOD tod

fromTOD :: Time.TimeOfDay -> TimeOfDay
fromTOD tod = rowOf (Time.todHour tod)
            & rowOf (Time.todMin tod)
            & rowOf (Time.todSec tod)

showTime :: (Has Day a, Has Hour a, Has Minute a, Has Second a)
         => a -> String
showTime time = unwords [
                  Time.showGregorian (Time.ModifiedJulianDay (Day ^. time))
                , List.intercalate ":" [
                    show2 . (Hour ^.) $ time
                  , show2 . (Minute ^.) $ time
                  , show2 . round . (Second ^.) $ time
                  ]
                ]
  where
    show2 x | 0 <= x && x <= 9 = '0' : show x
            | otherwise        = show x
