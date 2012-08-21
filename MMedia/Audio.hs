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



type AudioSample = Float
type SampleCnt = [AudioSample]

data Sampling = PCM { pcmSampleDuration :: RelTime }    -- sample duration / inverse sample rate

data AudioChunk = AudioChunk { -- aChunkLength :: RelTime
                               renderAudioChunk :: Sampling -> SampleCnt
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
   let chunkGen tᵢ (PCM δs) = [ f t | t <- takeWhile(`earlierThan` tᵢ@+%δt)
                                                $ iterate(@+% δs) tᵢ ]
   in  [ AudioChunk $ chunkGen tᵢ | tᵢ <- iterate(@+% δt) t₀ ]


instance Chunky AudioChunk where
  switchOvr t (AudioChunk cr₁) (AudioChunk cr₂)
     = AudioChunk crᵣ
    where crᵣ sl@(PCM rate) = take lsples (cr₁ sl) ++ drop lsples (cr₂ sl)
            where lsples = floor $ t %/% rate


instance Mixable AudioChunk where
  mixChunks = AudioChunk . mix
    where mix chunks = map sum . transpose . flip map chunks . flip renderAudioChunk