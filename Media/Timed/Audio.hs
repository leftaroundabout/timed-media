-- |
-- Module      : Media.Timed.Audio
-- Copyright   : (c) Justus Sagemüller 2012
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemuej $ smail.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
-- 
-- Audio (or generally /sound/, it is not in principle limited to the human hearing
-- range) is actually just a single physical quantity (in reality pressure)
-- that varies in time, i.e. a function \"@p :: Time -> Pressure@\".
-- This is very naturally expressed by the 'Timeline'
-- approach. The time-chunks are, as usual in DSP, themselves containers
-- for discretely time-sampled values of /p/; the sampling rate (and /kind of sampling/,
-- for that matter) is a chunk-generation parameter, i.e. the evaluator of
-- the audio data can request what sample rate (and thus, in a rather rough sense,
-- /quality/) of audio they need.


module Media.Timed.Audio( -- * Imports
                     module Media.Timed.Timeline
                   , -- * Types
                     Audio
                   , AudioSample
                   , SampleDuration
                   , SampleArray
                   , SampleCnt(SampleCnt, sampleCntSampling, getSampleCnt)
                   , Sampling(PCM, pcmSampleDuration)
                   , fromPCM
                   , AudioChunk(AudioChunk, renderAudioChunk)
                   , AudioRendering
                   , -- * Basic audio generation and processing
                     silence, silentChunks
                   , ResamplingMode( Oversample_Naïve, naïveOversamplingRatio
                                   , Oversample_Averaging, avgOversamplingRatio
                                   , Resample_Affine, affinResample_AltSampling )
                   , resampled
                   , audioFn
                   ) where

import Media.Timed.Timecode.Arith
import Media.Timed.Timeline

import Media.Timed.MiscUtil.Data

import Data.List
import Data.Maybe
import Data.Fixed

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



audioChunkMap :: (SampleCnt->SampleCnt) -> AudioChunk -> AudioChunk
audioChunkMap f (AudioChunk c) = AudioChunk $ fmap f c

audioSampleCntMap :: (SampleArray->SampleArray) -> AudioChunk -> AudioChunk
audioSampleCntMap f = audioChunkMap $ \(SampleCnt sl cnt) -> SampleCnt sl $ f cnt

-- fromPCMchunkGen :: (RelTime -> SampleCnt) -> AudioChunk
-- fromPCMchunkGen gen = AudioChunk . Just $ gen . pcmSampleDuration


-- | As said above, the evaluator may specify in which way each chunk is supposed
-- to be sampled. The chunk length should always be an integer multiple of the sample
-- duration, otherwise funny skidding effects might occur.

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


silentChunks :: TimeRendering Sampling AudioChunk
silentChunks = TimeRendering $ \ _ -> (AudioChunk Nothing, silentChunks)



-- | While it is often sufficient to use one sample rate throughout the processing
-- of an entire project, there may be occasions where it becomes necessary to
-- convert. Most obviously, audio files are not adaptive, but also different
-- audio effects tend to need different amounts of \"frequency headroom\"
-- to work well, for instance distortion effects may produce audible aliasing unless
-- used with sufficient headroom.

data ResamplingMode = 
                        -- | Oversample by simply (forwards) repeating each sample
                        -- /n/ times \/ (backwards) dropping all but each /n/-th
                        -- sample in a PCM stream. Avoid this, there is hardly a
                        -- performance advantage over the already much better
                        -- 'Oversample_Averaging'.
                      Oversample_Naïve { naïveOversamplingRatio :: Int }
                    
                        -- | Oversample by (forwards) linear interpolation \/
                        -- (backwards) averaging over /n/ samples. In the former case
                        -- it is basically a specialisation of 'Resample_Affine', but
                        -- works much better.
                    | Oversample_Averaging { avgOversamplingRatio :: Int }
                    
                        -- | Resample to\/from one specific sampling rate, using
                        -- simple linear (or, /affine/) interpolation between
                        -- the two samples of the source that are closest to the
                        -- resulting one in the output.
                        -- Intended as a simple, but not really good way of bridging
                        -- to a source with fixed sample rate. (Generally less
                        -- efficient than the integral-factor oversamplings, and
                        -- worse in quality than even 'Oversample_Averaging').
                    | Resample_Affine { affinResample_AltSampling :: SampleDuration }
               {-
                    | Oversample_SincBL { aAOversampling :: Int      -- over-samples per output sample
                                        , aAOvrspleSincWidth :: Int  -- output samples included in the bandlimiting
                                        }
                    | RandSample_LinInterpolate
                    | RandSample_SincInterpolate { aARandSampleSincWidth :: Int }
                    | RandOverSample_SincBL { aARandOversampling :: Int
                                            , aARandOvrspleSincWidth :: Int }
                                                                                   -}


-- | Perhaps somewhat confusingly, 'resampled' /takes/ audio that was
-- created with a designated (e.g. higher by some factor or fixed to a
-- particular value) sampling rate and converts it to whatever
-- rate the evaluator of the resulting 'Audio' requests. /Resampling/
-- /from fixed sample rate is currently only supported for chunk lengths/
-- /a multiple of the sampling duration, otherwise a runtime error/
-- /is raised!/ (FIXME)

resampled :: ResamplingMode -> Audio -> Audio

resampled (Oversample_Naïve 1) = id
resampled (Oversample_Naïve n) = fromPCM . cmap' g
 where g (PCM δs) = (PCM $ δs%/n, audioSampleCntMap νOversple)
       νOversple chunk = VU.unsafeBackpermute chunk
                             . VU.enumFromStepN 0 n $ VU.length chunk `div` n

resampled (Oversample_Averaging 1) = id
resampled (Oversample_Averaging n) = fromPCM . cmap' g
 where g (PCM δs) = (PCM $ δs%/n, audioSampleCntMap aOversple)
       aOversple chunk = VU.generate (VU.length chunk `div` n) 
                             (\i -> VU.sum (VU.slice (i*n) n chunk)
                                       / fromIntegral n             )

resampled (Resample_Affine δs) = fromPCM . \(Timeline line)
 -> Timeline $ \δt t₀ tpre
  -> if δs`divides`δt
      then let (TimePresentation (TimeRendering rend) nPreload) = line δt t₀ tpre
           in  TimePresentation (TimeRendering $ rlOversple δt rend) nPreload
      else error $
             "Requested chunk length " ++ show δt
          ++ " that is not a multiple of fixed sample duration " ++ show δs ++ "."
 where rlOversple δt rend cgp@(PCM δs')
        | δs'==δs    = rend cgp
        | otherwise  = let (aChunk, TimeRendering cont) = rend (PCM δs)
                       in  ( audioChunkMap f aChunk
                           , TimeRendering $ rlOversple δt cont )
         where f (SampleCnt (PCM δsᵣ) chnk)
                 | δsᵣ == δs
                     = SampleCnt (PCM δs') . VU.generate n' $ 
                           \i -> let i'₀ = floor i'ᵣ
                                     i'ᵣ = fromIntegral i * q
                                     η = i'ᵣ - fromIntegral i'₀  :: Float
                                 in  (chnk `VU.unsafeIndex` i'₀) * (1-η)
                                      + (chnk `VU.unsafeIndex` (i'₀+1)) * η
                 | otherwise  = error "Chunk came back with sample rate other than requested."
               q = realToFrac $ δs' %/% δs
               n = round $ δt %/% δs
               n' = round $ δt %/% δs'
              




divides :: RelTime -> RelTime -> Bool
t `divides` t' = abs remain / quotient < 10^^(-6)
 where ratio = t' %/% t
       quotient = fromIntegral $ round ratio
       remain = ratio - quotient



audioFn :: (Timecode -> AudioSample) -> Audio
audioFn f = fromPCM $ Timeline build
 where build δt t₀ tpre
          = TimePresentation ( chunkGen $ t₀ @-% nPreload *% δt )
                             nPreload
        where chunkGen tᵢ = TimeRendering $ \(PCM δs)
                              -> ( AudioChunk . Just $ SampleCnt
                                       (PCM δs)
                                       ( VU.generate (round $ δt %/% δs)
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
                            lsples = round $ t %/% δs
                 Nothing  
                   -> SampleCnt sl $ lslice VU.++ rslice 
                      where lslice = VU.take lsples arr₁
                            (SampleCnt sl@(PCM δs) arr₁) = fromJust cr₁  -- cr₁ can't be Nothing here, because of the first patter for switchOvr
                            rslice =  VU.replicate (VU.length arr₁ - lsples) 0
                            lsples = round $ t %/% δs


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
  gainChunk g = audioSampleCntMap (VU.map (*g))
