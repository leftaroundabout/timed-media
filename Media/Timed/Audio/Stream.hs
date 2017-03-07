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



data Audio = Audio {
    audioStreamLengthExponent :: Int  -- ^ 2**time in seconds
  , audioSignal :: UnitL2 Int Double
  }

