-- |
-- Module      : Media.Timed.Audio.Channels
-- Copyright   : (c) Justus Sagemüller 2012
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemuej $ smail.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
-- 

module Media.Timed.Audio.Channels where


import Media.Timed.Multiplex
import Media.Timed.Multiplex.HList
import Media.Timed.Audio

import Data.HList.HListPrelude


type DemuxedStereoAudio = HCons Audio (
                          HCons Audio ( HNil ) )
type StereoAudio = Timeline (MultiplexParams DemuxedStereoAudio)
                            (MultiplexChunks DemuxedStereoAudio)


data Panning = PanLawConstPower { panLawConstPwr_angle :: Float }

panHardLeft    = PanLawConstPower (-pi/2)
panHalfLeft    = PanLawConstPower (-pi/4)
panSlightLeft  = PanLawConstPower (-pi/8)
panCenter      = PanLawConstPower 0
panSlightRight = PanLawConstPower (pi/8)
panHalfRight   = PanLawConstPower (pi/4)
panHardRight   = PanLawConstPower (pi/2)


panToLRGain :: Panning -> (Gain, Gain)
panToLRGain (PanLawConstPower ϑ) = (sin ϑ', cos ϑ')
 where ϑ' = ϑ/2 + pi/4


stereoLRPan :: Panning -> Audio -> StereoAudio
stereoLRPan pan (Timeline monoLine) = Timeline stereoLRLine
 where stereoLRLine δt t₀ tpre
          = TimePresentation ( TimeRendering $ renderPan monoRender ) nPre
         where TimePresentation monoRender nPre = monoLine δt t₀ tpre
       
       renderPan monoRender (HCons lParam (HCons rParam HNil))
         | lParam == rParam  = let (monoChunk, cont) = runTimeRender monoRender lParam
                               in  ( HCons (gainChunk lGain monoChunk) (
                                     HCons (gainChunk rGain monoChunk) HNil )
                                   , TimeRendering $ renderPan cont           )
         | otherwise         = let (lChunk, cont) = runTimeRender monoRender lParam
                                   (rChunk, _)    = runTimeRender monoRender rParam
                               in  ( HCons (gainChunk lGain lChunk) (
                                     HCons (gainChunk rGain rChunk) HNil )
                                   , TimeRendering $ renderPan cont        )
       
       (lGain,rGain) = panToLRGain pan


