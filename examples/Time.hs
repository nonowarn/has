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

type TimeOfDay = FieldOf Hour :&: FieldOf Minute :&: FieldOf Second

type Time = FieldOf Day :&: FieldOf Hour :&: FieldOf Minute :&: FieldOf Second

type ZonedTime = FieldOf TimeZone :&: FieldOf Day
    :&: FieldOf Hour :&: FieldOf Minute :&: FieldOf Second

-- But that was too verbose
-- You can write same type as follows
-- > type Time = FieldOf Day :&: TimeOfDay
-- > type ZonedTime = FieldOf TimeZone :&: Time

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
getTod a = fieldOf (Hour ^. a)
         & fieldOf (Minute ^. a)
         & fieldOf (Second ^. a)

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
    in fieldOf (Time.toModifiedJulianDay (Time.utctDay utctime))
     & fromTOD tod

fromZonedTime :: Time.ZonedTime -> ZonedTime
fromZonedTime zonedtime =
    let localtime = Time.zonedTimeToLocalTime zonedtime
        day       = Time.toModifiedJulianDay $ Time.localDay localtime
        tod       = Time.localTimeOfDay localtime
    in fieldOf (Time.zonedTimeZone zonedtime)
     & fieldOf day
     & fromTOD tod

fromTOD :: Time.TimeOfDay -> TimeOfDay
fromTOD tod = fieldOf (Time.todHour tod)
            & fieldOf (Time.todMin tod)
            & fieldOf (Time.todSec tod)

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
