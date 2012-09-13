{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
-- |
-- Module      : MMedia.Audio.FX.Filter.SimpleIIR
-- Copyright   : (c) Justus Sagemüller 2012
-- License     : GPL v3
-- 
-- Maintainer  : sagemuej@smail.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
-- 
-- Static IIRs (/infinite impulse response/, although they are actually
-- assumed to relaxate – save for an insignificant difference – in a finite
-- time interval, so the infinite response may be trimmed down to a finite one)
-- can be used to implement most commonly used filters, such as high\/low\/bandpass,
-- notches, band boosts etc.. They work by iterating a recurrence relation
-- with some internal accumulated state over PCM-sampled audio data.
-- 'staticIIR' is thus basically an accelerated 'mapAccumL' over audio samples.


module MMedia.Audio.FX.Filter.SimpleIIR( -- * Generic IIR filter generation
                                         IIRGenStatic( IIRGenStatic
                                                     , statIIR_RelaxTime
                                                     , statIIR_ImpulseFollower
                                                     , statIIR_CarryInit       )
                                       , staticIIR
                                       , -- * Basic filter types
                                         lpOrder1
                                       , lpOrder2
                                       , lpOrder2Nonlinear
                                       , lpOrder2Saturating
                                       ) where


import MMedia.Timecode.Arith
import MMedia.Timeline
import MMedia.Audio

import Data.List
import Data.Maybe
import qualified Data.Vector.Storable as VU

import Control.Monad.ST
import qualified Data.Vector.Storable.Mutable as VM
import Data.STRef
import Control.Monad

import Data.Complex



data IIRGenStatic carry
   = IIRGenStatic { statIIR_RelaxTime :: RelTime     -- ^ how long the system needs to run until it behaves as if it had run /always/, i.e. until the deviation to that state becomes insignificant.
                  , statIIR_ImpulseFollower ::
                       ( SampleDuration ->
                           carry -> AudioSample -> (carry, AudioSample) )
                      -- ^ the impulse-sample-follower. Should be properly curried, i.e. as much as possible of the calculations should be carried out as soon as the 'SampleDuration' argument is received, since this value will be used for a whole chunk.
                  , statIIR_CarryInit :: carry   -- ^ the IIRs state should be initialised with the \"silence-state\", since that is also what will be assumed for \"silent audio chunks\".
                  }



type ASampleProc carry = forall s. Int -> ST s carry -> ST s (carry, AudioSample)

staticIIR :: forall carry.
                IIRGenStatic carry -> Audio -> Audio
staticIIR ( IIRGenStatic tᵣ fᵢ c₀ ) srcAudio = fromPCM $ Timeline fLine
 where 
       fLine :: RelTime -> Timecode -> RelTime -> TimePresentation Sampling AudioChunk
       fLine δt t₀ tpre = TimePresentation 
                              ( TimeRendering $ render source (c₀,tᵣ) )
                              nPreload
        where 
              (TimePresentation source nPreload)
                   = runTimeline srcAudio δt t₀ (tpre %+% tᵣ)
              
              render :: AudioRendering -> (carry, RelTime) -> Sampling
                              -> (AudioChunk, AudioRendering)
              render src (cr, tSilent) (PCM δs)
                                = (chnk', TimeRendering $ render cont cr')
               where (chnk, cont) = runTimeRender src $ PCM δs
                     (cr', chnk') = filtR (fᵢ δs) δs cr tSilent chnk
              
              filtR :: (carry -> AudioSample -> (carry, AudioSample))
                         -> SampleDuration -> carry -> RelTime -> AudioChunk
                            -> ((carry, RelTime), AudioChunk)
              filtR fᵣ δs cr tSilent (AudioChunk(Just chunk))
                                = ((cr', noTime), chunkPkg δs chunk')
                                   where (cr', chunk') = fKernel nSamples proc cr
                                         sig = getSampleCnt chunk
                                         nSamples = VU.length sig
                                         proc :: ASampleProc carry
                                         proc = liftM2 (flip fᵣ) . VU.unsafeIndexM sig
              filtR fᵣ δs cr tSilent (AudioChunk Nothing)
                | tSilent > tᵣ  = ((cr, tSilent), AudioChunk Nothing)
                | otherwise     = ((cr', tSilent %+% δt), chunkPkg δs chunk')
                                   where (cr', chunk') = fKernel nSamples proc cr
                                         nSamples = (floor $ δt %/% δs)
                                         proc :: ASampleProc carry
                                         proc = const. fmap $ flip fᵣ 0
              
              fKernel :: Int -> ASampleProc carry
                            -> carry -> (carry, SampleArray)
              fKernel nSamples proc cr = runST $ do
                         result <- VM.new nSamples
                         carry <- newSTRef cr
                         forM_ [0 .. nSamples-1] (\i -> do
                            (c', sr) <- proc i (readSTRef carry)
                            VM.unsafeWrite result i sr
                            writeSTRef carry c'
                          )
                         carryRes <- readSTRef carry
                         audioRes <- VU.freeze result
                         return (carryRes, audioRes)
              
              chunkPkg :: SampleDuration -> SampleArray -> AudioChunk
              chunkPkg δs chunk = AudioChunk . Just $ SampleCnt
                                     { sampleCntSampling = PCM δs
                                     , getSampleCnt = chunk       }

{-# INLINE staticIIR #-}

--                          let nSamples = (floor $ δt %/% δs)
--                          result <- VM.new nSamples
--                          carry <- newSTRef cr
--                          forM_ [0 .. nSamples-1] (\i -> do
--                             (c', sr) <- liftM2 (fᵢ δs) (readSTRef carry)
--                                                        (return 0)
--                             VM.unsafeWrite result i sr
--                             writeSTRef carry c'
--                           )
--                          carryRes <- readSTRef carry
--                          audioRes <- VU.freeze result
--                          return (carryRes, audioRes)

--                where 
--                      filtAccC :: (carry, RelTime) -> AudioChunk
--                                -> ((carry, RelTime), AudioChunk)
--                      filtAccC (cr, _) (AudioChunk (Just chunk))
--                                          = tRepack (filtAccCRun cr chunk) noTime
--                      filtAccC (cr, tSilent) (AudioChunk Nothing)
--                          | tSilent > tᵣ  = ((c₀, tSilent), AudioChunk Nothing)
--                          | otherwise     = tRepack (filtAccCRun0 cr) (tSilent %+% δt)
--                      
-- 
--                      filtAccCRun :: carry -> SampleCnt
--                                      -> (carry, AudioChunk)
--                      filtAccCRun cr chunkGen
--                         = chGenRepack $ \ δs -> runST $ do
--                              let sig = chunkGen $ PCM δs
--                              let nSamples = VU.length sig
--                              result <- VM.new nSamples
--                              carry <- newSTRef cr
--                              forM_ [0 .. nSamples-1] (\i -> do
--                                 (c', sr) <- liftM2 (fᵢ δs) (readSTRef carry)
--                                                            (VU.unsafeIndexM sig i)
--                                 VM.unsafeWrite result i sr
--                                 writeSTRef carry c'
--                               )
--                              carryRes <- readSTRef carry
--                              audioRes <- VU.freeze result
--                              return (carryRes, audioRes)
--                      filtAccCRun0 :: carry -> (carry, AudioChunk)
--                      filtAccCRun0 cr
--                         = chGenRepack $ \ δs -> runST $ do
--                              let nSamples = (floor $ δt %/% δs)
--                              result <- VM.new nSamples
--                              carry <- newSTRef cr
--                              forM_ [0 .. nSamples-1] (\i -> do
--                                 (c', sr) <- fmap (flip (fᵢ δs) 0) (readSTRef carry)
--                                 VM.unsafeWrite result i sr
--                                 writeSTRef carry c'
--                               )
--                              carryRes <- readSTRef carry
--                              audioRes <- VU.freeze result
--                              return (carryRes, audioRes)
-- --                              liftM2 (curry return) (readSTRef carry)
-- --                                                    (VU.freeze result)
-- 
--                      tRepack (a,b) c = ((a,c),b)
--                      chGenRepack :: (RelTime -> (carry, SampleCnt)) -> (carry, AudioChunk)
--                      chGenRepack gen = (\(cc, chk) -> (cc, AudioChunk $ Just chk))
--                                    $ gen . pcmSampleDuration


--                      t₀carry :: carry
--                      line', pre' :: [AudioChunk]
--                      t₀Silencet :: RelTime
--                      
--                      ((t₀carry, t₀Silencet), pre')
--                        | any (isJust . renderAudioChunk) limtdPre
--                                         = mapAccumR filtAccC (c₀, noTime) limtdPre
--                        | otherwise      = ((c₀, tᵣ%* 2), limtdPre)
--                      (_, line') = mapAccumL filtAccC (t₀carry, t₀Silencet) line
-- 


infixl 7 ⋅
(⋅) :: Fractional a => a->a->a; (⋅) = (*)


-- | A simple first-order lowpass filter: 6 dB per octave rolloff for ν ≫ νᵥ,
--   where νᵥ is the cutoff frequency, i.e. the frequency at which @'lpOrder1' νᵥ aud@
--   is by 3 dB quieter than @aud@ (in other words, the amplitude is there scaled
--   by a factor of √½).

lpOrder1 :: Frequency -> Audio -> Audio
lpOrder1 νᵥ = staticIIR $ IIRGenStatic tᵣ fᵢ c₀
 where tᵣ = 8 /~ νᵥ
       c₀ = 0 :: AudioSample
       fᵢ δs = f
        where f yⱼ₁ xⱼ = (yⱼ, yⱼ)
               where yⱼ = yⱼ₁ + λ ⋅ (xⱼ - yⱼ₁)
              λ = realToFrac . lp1stCoeff $ 2*pi * (νᵥ ~*% δs)

-- -- -- -- DERIVATION OF THE MATHEMATICS OF THIS FILTER -- -- -- --
-- Differential equation for continuous-time 1st-order low pass:
-- ∂y/∂t = -λ ⋅ (y(t) - x(t))
-- ◁▭▷ (iω + λ) ⋅ y(ω) = λ ⋅ x(ω)
-- |y(ωₑ)| =! |x(ωₑ)|/√2
-- ◁▭▷ ωₑ² + λ² = 2 ⋅ λ²
-- ◁▭▷ ωₑ = λ ◁▭▷ λ = 2πνₑ
-- 
-- Discrete version (sample rate 1):   ( One would normally just use the bilinear
--                                       transform here; this is a different (and
--                                       AFAICS more accurate) approach           )
-- y n - y (n-1) = λ ⋅ (x n - y (n-1))
-- φ ω - φ ω ⋅ exp(-i⋅ω) = λ ⋅ (1 - φ ω ⋅ exp(-i⋅ω))
-- ◁▭▷ λ = φ ω ⋅ (1 - exp(-i⋅ω))/(1 - φ ω ⋅ exp(-i⋅ω))
--   { for ω≪1, φ ω ≉ 1 (continuous limit):
--       λ ≈ φ ω ⋅ (1 - 1 + i⋅ω)/(1 - φ ω - O ω)
--   ,  with φ ωₑ = (1 - i)/2   ▭▷   λ ≈ ωₑ   ✔ }
-- We want |φ ωₑ| = √½, so put in φ ωₑ ≡ √½ ⋅ exp(i⋅ϑ). This yields in general
-- λ∉ℝ, we start from ϑ = -π/4 and correct this with 3 Newton-Raphson steps, then
-- disregard the remaining imaginary part.
-- λ = √½ ⋅ exp(i⋅ϑ) ⋅ (1 - exp(-i⋅ω))/(1 - √½ ⋅ exp(i⋅(ϑ-ω)))
-- ◁▭▷ ∂λ/∂ϑ = √2/2 ⋅ i⋅exp(i⋅ϑ) ⋅ (1 - exp(-i⋅ω))/(1 - √½ ⋅ exp(i⋅(ϑ-ω)))
--            - 1/2 ⋅ exp(i⋅ϑ) ⋅ i⋅exp(i⋅(ϑ-ω)) ⋅ (1 - exp(-i⋅ω))/(1 - √½ ⋅ exp(i⋅(ϑ-ω)))²



lp1stCoeff :: Double -> Double
lp1stCoeff ω = realPart $ λ !! 3
  where λ :: [Complex Double]
        λ = [isq2 ⋅ cis(ϑ!!j) ⋅ (1 - cis(-ω))/(1 - isq2 ⋅ cis(ϑ!!j-ω)) | j<-[0..]]

        ϑ :: [Double]
        ϑ = -pi/4 : [ϑ' j | j<-[1..]]
         where ϑ' j = ϑ!!(j-1) - imagPart (λ !! (j-1)) / imagPart (dλBYdϑ $ ϑ!!(j-1))

        dλBYdϑ :: Double -> Complex Double
        dλBYdϑ ϑⱼ = isq2 ⋅ i⋅cis ϑⱼ ⋅ (1 - cis(-ω))/(1 - isq2 ⋅ cis(ϑⱼ-ω))
                     - 1/2 ⋅ cis ϑⱼ ⋅ i⋅cis(ϑⱼ-ω) ⋅ (1 - cis(-ω))/(1 - isq2 ⋅ cis(ϑⱼ-ω))^2

        isq2 = sqrt 2/2 :: Complex Double
        i = 0:+1 :: Complex Double



-- | A second-order lowpass filter (state variable filter): 12 dB per octave
-- rolloff for ν ≫ νᵥ; at νᵥ the resonance is determined by the gain parameter,
-- which must be in range ]½ .. ∞[.

-- This one follows the normal derivation with bilinear transform, but something
-- is wrong apparently: these filters to not behave invariant under resampling.

lpOrder2 :: Gain -> Frequency -> Audio -> Audio
lpOrder2 q νᵥ = staticIIR $ IIRGenStatic tᵣ fᵢ c₀
 where tᵣ = (8 * q) /~ νᵥ
       qinv = 1/q
       c₀ = (0 :: AudioSample, 0 :: AudioSample)
       fᵢ δs = f
        where f (wⱼ₁,yⱼ₁) xⱼ = ((wⱼ, yⱼ), yⱼ)
               where yⱼ = yⱼ₁ + λ ⋅ wⱼ₁
                     wⱼ = λ ⋅ sⱼ + wⱼ₁
                     sⱼ = xⱼ - qinv * wⱼ₁ - yⱼ
                     
              λ = 2 * sin(pi * realToFrac(νᵥ ~*% δs))


-- | State-variable filter like 'lpOrder2', but allows injection of a nonlinear
-- \"saturator function\" into the resonance part of the circuit. Careful, this may
-- easily make the filter unstable! – suitable functions should approach identity
-- for small values and have derivative ∊ [0..1] everywhere.
-- Also, at the moment it only works properly when using high oversampling, as
-- the implementation is taken from the linear version of the filter,
-- which is only tuned correctly without the nonlinear function.

lpOrder2Nonlinear :: Gain -> (AudioSample->AudioSample) -> Frequency -> Audio -> Audio
lpOrder2Nonlinear q satfn νᵥ = staticIIR $ IIRGenStatic tᵣ fᵢ c₀
 where tᵣ = (8 * q) /~ νᵥ
       qinv = 1/q
       c₀ = (0 :: AudioSample, 0 :: AudioSample)
       fᵢ δs = f
        where f (wⱼ₁,yⱼ₁) xⱼ = ((wⱼ, yⱼ), yⱼ)
               where yⱼ = yⱼ₁ + λ ⋅ wⱼ₁
                     wⱼ = satfn $ λ ⋅ sⱼ + wⱼ₁
                     sⱼ = xⱼ - qinv * wⱼ₁ - yⱼ
                     
              λ = 2 * sin(pi * realToFrac(νᵥ ~*% δs))


-- | 'lpOrder2Nonlinear' with a saturation function that softly clips (symmetrically)
-- peaks at 0 dB.

lpOrder2Saturating :: Gain -> Frequency -> Audio -> Audio
lpOrder2Saturating q = lpOrder2Nonlinear q satfn
 where satfn x
        | x>2        = 1
        | x<(-2)     = -1
        | otherwise  = x / (1 + (x/2)^2)
