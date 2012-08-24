module MMedia.Timecode ( Timecode(), timecode, RelTime(), Frequency(), seconds, hertzs
                       , noTime, oneSecond, oneHertz, timeZero
                       ) where

import MMedia.Timecode.Internal

timecode :: [Double] -> Timecode
timecode (hh:mm:[ss]) = Timecode $ 3600*hh + 60*mm + ss
timecode    (mm:[ss]) = Timecode $ 60*mm + ss
timecode        [ss]  = Timecode ss

-- instance Show RelTime where
--   show (RelTime t) = show(


noTime :: RelTime
noTime = RelTime 0

oneSecond :: RelTime
oneSecond = RelTime 1

timeZero :: Timecode
timeZero = Timecode 0

oneHertz :: Frequency
oneHertz = Frequency 1
