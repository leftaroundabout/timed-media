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
{-# LANGUAGE ScopedTypeVariables #-}

module Media.Timed.Audio.Stream where


import Math.FunctionalAnalysis.L2Function.R1
import qualified Data.Vector.Generic as Arr

import qualified Sound.Pulse.Simple as PA

import qualified Sound.File.Sndfile as SF
import qualified Sound.File.Sndfile.Buffer.Vector as BV

import Data.Default
import Data.Monoid


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


loadAudio :: FilePath -> IO [Audio]
loadAudio fPath = do
    (SF.Info nFr sr nChan _ _ _, Just (buf :: BV.Buffer Double)) <- SF.readFile fPath
    let bufv = BV.fromBuffer buf
        nSpl = Arr.length bufv
        len | nSpl == nFr * nChan
          = fromIntegral nFr / fromIntegral sr
        ldlAudio = ceiling $ logBase 2 len
        nTot = sr * 2^ldlAudio
        silence = Arr.replicate (nTot-nFr) 0
    return [ Audio ldlAudio . fromUniformSampled def $ chanv <> silence
           | i <- [0..nChan-1]
           , let chanv = Arr.generate nFr $ \j -> bufv Arr.! (j*nChan + i)
           ]
