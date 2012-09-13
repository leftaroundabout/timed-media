-- |
-- Module      : Media.Timed.Timecode
-- Copyright   : (c) Justus Sagemüller 2012
-- License     : GPL v3
-- 
-- Maintainer  : sagemuej@smail.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
-- 
-- Absolute- and relative-time types, for handling these physical quantities
-- in a way free of noncanonicalties, guaranteed by the type system. Calculations
-- can be done with the operations from "Media.Timed.Timecode.Arith".

module Media.Timed.Timecode ( Timecode(), timecode, timeDecode, RelTime(), Frequency(), seconds, hertzs
                       , noTime, oneSecond, oneHertz, timeZero
                       ) where

import Media.Timed.Timecode.Internal
import Data.Fixed

import Numeric

-- | 'timecode' and 'timeDecode' rather /ab/use Haskell list syntax: a time meaning
-- @hh@ hours plus @mm@ minutes plus @ss@ seconds from reference zero is represented
-- by a list @[hh,mm,ss]@, or as it should always be written @hh:mm:[ss]@, which
-- is still a bit awkward but looks quite pleasantly like the standard way of
-- writing time codes. It may as well be @mm:[ss]@ or @[ss]@, but the list must never
-- be empty nor contain more than 3 elements. Though all elements must obviously be
-- floating-point, all but the seconds should be integral values. /Only the most/
-- /significant number/ may be negative, which then means the entire time code is
-- a \"negative time\".

timecode :: [Double] -> Timecode
timecode (fs:r) | fs<0   =  Timecode $ - getTimecode(timecode (-fs:r))
timecode (hh:mm:[ss]) = Timecode $ 3600*hh + 60*mm + ss
timecode    (mm:[ss]) = Timecode $ 60*mm + ss
timecode        [ss]  = Timecode ss

timeDecode :: Timecode -> [Double]
timeDecode (Timecode t)
 | t<0        = let (mxs:lss) = timeDecode $ Timecode (-t)
                in  if mxs>0 then (-mxs):lss
                             else let (mxs':lss') = lss in (-mxs'):lss'
 | t>=3600    = let (hh,r) = t`divMod'`3600
                in  realToFrac hh : timeDecode (Timecode r)
 | otherwise  = let (mm,r) = t`divMod'`60
                in  realToFrac mm : [r]
         

instance Show Timecode where
  show t = "timecode(" ++ case timeDecode t of
                           (hh:mm:[ss]) ->          show(round hh)
                                          ++ ":" ++ show(round mm)
                                          ++ ":["++ showRound' ss ++ "]"
                           (mm:[ss])    ->          show(round mm)
                                          ++ ":["++ showRound' ss ++ "]"
                           [ss]         ->    "["++ showRound' ss ++ "]"
           ++ ")"

showRound' n = showFFloat (Just 3) n ""

instance Show RelTime where
  show (RelTime t) = show t ++ "*%oneSecond"


noTime :: RelTime
noTime = RelTime 0

oneSecond :: RelTime
oneSecond = RelTime 1

timeZero :: Timecode
timeZero = Timecode 0

oneHertz :: Frequency
oneHertz = Frequency 1
