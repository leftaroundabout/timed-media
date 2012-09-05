-- |
-- Module      : MMedia.Audio
-- Copyright   : (c) Justus Sagemüller 2012
-- License     : GPL v3
-- 
-- Maintainer  : sagemuej@smail.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
-- 
module MMedia.Audio( module MMedia.Timeline
                   , AudioSample
                   , SampleDuration
                   , SampleArray
                   , SampleCnt(SampleCnt, sampleCntSampling, getSampleCnt)
                   , Sampling(PCM, pcmSampleDuration)
                   , AudioChunk(AudioChunk, renderAudioChunk)
                   , AudioRendering, Audio, fromPCM
                   , silence
                   , AntiAliasStrategy(NoAntiAliasing)
                   , audioFn
                   ) where

import MMedia.Timecode.Arith
import MMedia.Timeline

import MMedia.MiscUtil.Data

import Data.List
import Data.Maybe

import qualified Data.Vector.Storable as VU



type AudioSample = Float -- Double is already rather more common in modern audio
                         -- processing, but this is not really significantly better
                         -- than 32-bit floats since these already surpass the dynamic
                         -- range of the human ear by many orders of magnitude.


type SampleDuration = RelTime
data Sampling = PCM { pcmSampleDuration :: SampleDuration }  -- inverse sample rate


type SampleArray = VU.Vector AudioSample

data SampleCnt = SampleCnt { sampleCntSampling :: Sampling
                           , getSampleCnt :: SampleArray   }


newtype AudioChunk = AudioChunk { -- aChunkLength :: RelTime
                                  renderAudioChunk :: Maybe SampleCnt
                                }

-- fromPCMchunkGen :: (RelTime -> SampleCnt) -> AudioChunk
-- fromPCMchunkGen gen = AudioChunk . Just $ gen . pcmSampleDuration


type Audio = Timeline Sampling AudioChunk
type AudioRendering = TimeRendering Sampling AudioChunk


fromPCM :: Audio -> Audio
fromPCM = id


silence :: Audio
silence = Timeline $ \δt _ tpre -> TimePresentation silentChunks (ceiling $ tpre %/% δt)
 where silentChunks = TimeRendering $ \ _ -> (AudioChunk Nothing, silentChunks)


data AntiAliasStrategy = NoAntiAliasing {-
                       | Oversample_SincBL { aAOversampling :: Int      -- over-samples per output sample
                                           , aAOvrspleSincWidth :: Int  -- output samples included in the bandlimiting
                                           }
                       | RandSample_LinInterpolate
                       | RandSample_SincInterpolate { aARandSampleSincWidth :: Int }
                       | RandOverSample_SincBL { aARandOversampling :: Int
                                               , aARandOvrspleSincWidth :: Int }
                                        -}


audioFn :: AntiAliasStrategy -> (Timecode -> AudioSample) -> Audio
audioFn NoAntiAliasing f = fromPCM $ Timeline build
 where build δt t₀ tpre
          = TimePresentation ( chunkGen $ t₀ @-% nPreload *% δt )
                             nPreload
        where chunkGen tᵢ = TimeRendering $ \(PCM δs)
                              -> ( AudioChunk . Just $ SampleCnt
                                       (PCM δs)
                                       ( VU.generate (floor $ δt %/% δs)
                                                     ( \i -> f(tᵢ @+% δs %* i) ) )
                                 , chunkGen (tᵢ @+% δt)                            )
              nPreload
                | tpre>=noTime  = ceiling $ tpre %/% δt
                | otherwise     = error "Negative preload time requested for audioFn."


instance Chunky AudioChunk where
  switchOvr t (AudioChunk Nothing) (AudioChunk Nothing)
     = AudioChunk Nothing
  switchOvr t (AudioChunk cr₁) (AudioChunk cr₂)
     = AudioChunk $ Just crᵣ
    where crᵣ = case cr₂ of
                 Just (SampleCnt sl@(PCM δs) arr₂)
                   -> SampleCnt sl $ lslice VU.++ rslice
                      where lslice = case cr₁ of
                              Just (SampleCnt _ arr₁)
                                        -> VU.take lsples $ arr₁
                              Nothing   -> VU.replicate lsples 0
                            rslice = VU.drop lsples $ arr₂
                            lsples = floor $ t %/% δs
                 Nothing  
                   -> SampleCnt sl $ lslice VU.++ rslice 
                      where lslice = VU.take lsples arr₁
                            (SampleCnt sl@(PCM δs) arr₁) = fromJust cr₁  -- cr₁ can't be Nothing here, because of the first patter for switchOvr
                            rslice =  VU.replicate (VU.length arr₁ - lsples) 0
                            lsples = floor $ t %/% δs


instance Mixable AudioChunk where
  mixChunks chunks = AudioChunk mix
    where mix = foldl' combiner Nothing $ map renderAudioChunk chunks
          combiner = maybeCombine
                      ( \(SampleCnt sl chnk₁)
                         (SampleCnt sl' chnk₂)   -- assert(sl==sl')
                        -> SampleCnt sl $ VU.zipWith(+) chnk₁ chnk₂ )
--     where mix = foldl (VU.zipWith(+)) Nothing neChunks
--           neChunks = catMaybes $ map renderAudioChunk chunks


instance Gainable AudioChunk where
  gainChunk g (AudioChunk Nothing) = AudioChunk Nothing
  gainChunk g (AudioChunk (Just (SampleCnt spling source)))
            = (AudioChunk (Just (SampleCnt spling gained)))
   where gained = VU.map (*g) source
