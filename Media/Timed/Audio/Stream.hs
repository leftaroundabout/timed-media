-- |
-- Module      : Media.Timed.Audio.Stream
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
-- 

module Media.Timed.Audio.Stream where


import Math.FunctionalAnalysis.L2Function.R1
import qualified Data.Vector.Generic as Arr

import qualified Sound.Pulse.Simple as PA


data Audio = Audio {
    audioStreamLengthExponent :: Int  -- ^ Duration in seconds = 2**lengthExponent
  , audioSignal :: UnitL2 Int Double
  }


playMonoAudio :: Audio -> IO ()
playMonoAudio (Audio ldt signal) = do
    s <- PA.simpleNew Nothing "example" PA.Play Nothing "Sobolev audio signal"
          (PA.SampleSpec (PA.F32 PA.LittleEndian) 44100 1) Nothing Nothing
    PA.simpleWrite s . Arr.toList $ Arr.map (realToFrac::Double->Float) uniformSpldSig
    PA.simpleDrain s
    PA.simpleFree s
 where uniformSpldSig = toUniformSampled (round $ 44100 * 2^^ldt) signal
