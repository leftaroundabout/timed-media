module MMedia.Timecode ( Timecode(), timecode, RelTime(), Frequency(), seconds, hertzs
                       , oneSecond, oneHertz, timeZero
                       ) where

import MMedia.Timecode.Internal

timecode :: [Double] -> Timecode
timecode (hh:mm:[ss]) = Timecode $ 3600*hh + 60*mm + ss
timecode    (mm:[ss]) = Timecode $ 60*mm + ss
timecode        [ss]  = Timecode ss

-- instance Show RelTime where
--   show (RelTime t) = show(
