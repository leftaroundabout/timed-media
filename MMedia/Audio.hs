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
-- | Audio is actually just a single physical quantity (in reality pressure)
-- that varies in time, i.e. a function \"@p :: Time -> Pressure@\".
-- This is very naturally expressed by the 'Timeline'
-- approach. The time-chunks are, as usual in DSP, themselves containers
-- for discretely time-sampled values of /p/; the sampling rate (and /kind of sampling/,
-- for that matter) is a chunk-generation parameter, i.e. the evaluator of
-- the audio data can request what sample rate (and thus, in a rather rough sense,
-- /quality/) of audio they need.


module MMedia.Audio( -- * Imports
                     module MMedia.Timeline
                   , -- * Types
                     AudioSample
                   , SampleDuration
                   , SampleArray
                   , SampleCnt(SampleCnt, sampleCntSampling, getSampleCnt)
                   , Sampling(PCM, pcmSampleDuration)
                   , AudioChunk(AudioChunk, renderAudioChunk)
                   , AudioRendering, Audio, fromPCM
                   , -- * Basic audio generation and processing
                     silence
                   , ResamplingMode(Oversample_Naïve, Oversample_Averaging)
                   , oversampled
                   , audioFn
                   ) where

import MMedia.Timecode.Arith
import MMedia.Timeline

import MMedia.MiscUtil.Data

import Data.List
import Data.Maybe

import qualified Data.Vector.Storable as VU
-- import qualified Data.Vector.Storable.Mutable as VM



-- | Double is already rather more common in modern audio
-- processing, but this is not really significantly better
-- than 32-bit floats since these already surpass the dynamic
-- range of the human ear by many orders of magnitude. Since unboxed
-- arrays are used for storing audio samples, @Float@ has much lower
-- memory need.
type AudioSample = Float 


type SampleDuration = RelTime

-- | Currently, PCM is the only kind of sampling available. It might well stay
-- like that forever (most DAWs use exclusively PCM, it goes without saying),
-- but it's safer to keep options open. Use 'fromPCM' when
-- writing audio sources that can only produce PCM signals.

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


-- | \"Silent audio\" is handled as a special case: it consists exclusively of
-- chunks with @Nothing@ in them. Typical audio effects may completely shut down
-- their DSP load when encountering such chunks, which is particularly useful
-- in large projects where some effects only act on small audio events,
-- most of the time running idle.

silence :: Audio
silence = Timeline $ \δt _ tpre -> TimePresentation silentChunks (ceiling $ tpre %/% δt)
 where silentChunks = TimeRendering $ \ _ -> (AudioChunk Nothing, silentChunks)



-- | While it is often sufficient to use one sample rate throughout the processing
-- of an entire project, there may be occasions where it becomes necessary to
-- convert. Most obviously, audio files are not adaptive, but also different
-- audio effects tend to need different amounts of \"frequency headroom\"
-- to work well, for instance distortion effects may produce audible aliasing unless
-- used with sufficient headroom.

data ResamplingMode =   -- | Oversample by simply (forwards) repeating each sample _n_ times \/ (backwards) dropping all but each /n/-th sample in a PCM stream. Avoid this, there is hardly a performance advantage over the already much better 'Oversample_Averaging'.
                      Oversample_Naïve { naïveOversamplingRatio :: Int }
                        -- | Oversample by (forwards) linear interpolation \/ (backwards) averaging over _n_ samples
                    | Oversample_Averaging { avgOversamplingRatio :: Int }         
               {-
                    | Oversample_SincBL { aAOversampling :: Int      -- over-samples per output sample
                                        , aAOvrspleSincWidth :: Int  -- output samples included in the bandlimiting
                                        }
                    | RandSample_LinInterpolate
                    | RandSample_SincInterpolate { aARandSampleSincWidth :: Int }
                    | RandOverSample_SincBL { aARandOversampling :: Int
                                            , aARandOvrspleSincWidth :: Int }
                                                                                   -}


-- | Perhaps somewhat confusingly, 'oversampled' takes audio that was created with a
-- /high/ sampling rate and converts it down to a lower one.

oversampled :: ResamplingMode -> Audio -> Audio
oversampled (Oversample_Naïve 1) = id
oversampled (Oversample_Naïve n) = fromPCM . cmap' g
 where g (PCM δs) = (PCM $ δs%/n, νOversple)
       νOversple (AudioChunk Nothing) = AudioChunk Nothing
       νOversple (AudioChunk (Just (SampleCnt δs chunk)))
          = let res = VU.unsafeBackpermute chunk
                        . VU.enumFromStepN 0 n $ VU.length chunk `div` n
            in  AudioChunk . Just $ SampleCnt δs res
oversampled (Oversample_Averaging 1) = id
oversampled (Oversample_Averaging n) = fromPCM . cmap' g
 where g (PCM δs) = (PCM $ δs%/n, aOversple)
       aOversple (AudioChunk Nothing) = AudioChunk Nothing
       aOversple (AudioChunk (Just (SampleCnt δs chunk)))
          = let res = VU.generate (VU.length chunk `div` n) 
                        (\i -> VU.sum (VU.slice (i*n) n chunk)
                                  / fromIntegral n             )
            in  AudioChunk . Just $ SampleCnt δs res



audioFn :: (Timecode -> AudioSample) -> Audio
audioFn f = fromPCM $ Timeline build
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
