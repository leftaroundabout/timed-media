module MMedia.Audio( module MMedia.Timeline
                   , AudioSample
                   , SampleCnt
                   , Sampling(PCM), pcmSampleDuration
                   , AudioChunk(AudioChunk), renderAudioChunk
                   , Audio
                   , AntiAliasStrategy(NoAntiAliasing)
                   , audioFn
                   ) where

import MMedia.Timecode.Arith
import MMedia.Timeline

import Data.List
import Data.Maybe

import qualified Data.Vector.Unboxed as VU



type AudioSample = Float -- Double is rather more common in modern audio processing already,
                         -- but as far as I can see they are in no significant way better
                         -- than 32-bit floats, since these already surpass the dynamic
                         -- range of the human ear by many orders of magnitude.

type SampleCnt = VU.Vector AudioSample

data Sampling = PCM { pcmSampleDuration :: RelTime }    -- sample duration / inverse sample rate

data AudioChunk = AudioChunk { -- aChunkLength :: RelTime
                               renderAudioChunk :: Maybe (Sampling -> SampleCnt)
                             }


type Audio = Timeline AudioChunk



data AntiAliasStrategy = NoAntiAliasing
--                        | Oversample_SincBL { aAOversampling :: Int      -- over-samples per output sample
--                                            , aAOvrspleSincWidth :: Int  -- output samples included in the bandlimiting
--                                            }
--                        | RandSample_LinInterpolate
--                        | RandSample_SincInterpolate { aARandSampleSincWidth :: Int }
--                        | RandOverSample_SincBL { aARandOversampling :: Int
--                                                , aARandOvrspleSincWidth :: Int }


audioFn :: AntiAliasStrategy -> (Timecode -> AudioSample) -> Audio
audioFn NoAntiAliasing f = Timeline $ \δt t₀ ->
   let chunkGen tᵢ (PCM δs) = VU.generate (floor $ δt %/% δs)
                                          (\i -> f(tᵢ @+% δs %* i))
   in  [ AudioChunk . Just $ chunkGen tᵢ | tᵢ <- iterate(@+% δt) t₀ ]


instance Chunky AudioChunk where
  switchOvr _ (AudioChunk Nothing) (AudioChunk Nothing)
     = AudioChunk Nothing
  switchOvr t (AudioChunk cr₁) (AudioChunk cr₂)
     = AudioChunk $ Just crᵣ
    where crᵣ sl@(PCM rate) = case cr₂ of
                Just arr₂ -> lslice VU.++ rslice
                     where lslice = case cr₁ of
                             Just arr₁ -> VU.take lsples $ arr₁ sl
                             Nothing   -> VU.replicate lsples 0
                           rslice = VU.drop lsples $ arr₂ sl
                Nothing -> lslice VU.++ rslice 
                     where lslice = VU.take lsples arr₁
                           arr₁ = fromJust cr₁ sl
                           rslice =  VU.replicate (VU.length arr₁ - lsples) 0
            where lsples = floor $ t %/% rate


instance Mixable AudioChunk where
  mixChunks chunks = AudioChunk mix
    where mix = foldl' combiner Nothing $ map renderAudioChunk chunks
          combiner Nothing Nothing = Nothing
          combiner (Just chnk) Nothing = Just chnk
          combiner Nothing (Just chnk) = Just chnk
          combiner (Just chnk₁) (Just chnk₂)
             = Just $ \sl-> VU.zipWith(+) (chnk₁ sl) (chnk₂ sl)
--     where mix = foldl (VU.zipWith(+)) Nothing neChunks
--           neChunks = catMaybes $ map renderAudioChunk chunks


