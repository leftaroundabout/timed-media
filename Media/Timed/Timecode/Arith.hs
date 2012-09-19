-- |
-- Module      : Media.Timed.Timecode.Arith
-- Copyright   : (c) Justus Sagemüller 2012
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemuej $ smail.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
-- 
-- Simple arithmetic operations, @+@, @-@, @*@, @/@ have their usual meaning.
-- There is always an absolute time at the side of an @\@@,
-- always a relative time at the side of a @%@,
-- and always a frequency at the side of a @~@.

module Media.Timed.Timecode.Arith( module Media.Timed.Timecode 
                            , (@-@), (@+%), (@-%), (%+%), (%-%)
                            , (~+~), (~-~), (%+@), (/%), (%/%), (/~), (~/~), (~/)
                            , (~*%), (%*~), (%*), (%/), (~*), (*%), (*~)
                            , earlierThan, laterThan
                            )where

import Media.Timed.Timecode
import Media.Timed.Timecode.Internal


infix 6 @-@                
infixl 6 @+%, @-%           
infixl 6 %+%, %-%, ~+~, ~-~ 
infixr 6 %+@
infix 7 /%, %/%, /~, ~/~, ~*%, %*~
infixl 7 %*, %/, ~*
infixr 7 *%, *~

{-# INLINE (@-@) #-}
{-# INLINE (@+%) #-}
{-# INLINE (@-%) #-}
{-# INLINE (%+%) #-}
{-# INLINE (%-%) #-}
{-# INLINE (~+~) #-}
{-# INLINE (~-~) #-}
{-# INLINE (%+@) #-}
{-# INLINE (/%) #-}
{-# INLINE (%/%) #-}
{-# INLINE (/~) #-}
{-# INLINE (~/~) #-}
{-# INLINE (~/) #-}
{-# INLINE (~*%) #-}
{-# INLINE (%*~) #-}
{-# INLINE (%*) #-}
{-# INLINE (%/) #-}
{-# INLINE (~*) #-}
{-# INLINE (*%) #-}
{-# INLINE (*~) #-}

(@-@) :: Timecode -> Timecode -> RelTime
(%+@) :: RelTime -> Timecode -> Timecode
(@+%), (@-%) :: Timecode -> RelTime -> Timecode
(%-%), (%+%) :: RelTime -> RelTime -> RelTime
(~-~), (~+~) :: Frequency -> Frequency -> Frequency


Timecode t₁ @-@ Timecode t₂ = RelTime $ t₁ - t₂
RelTime δt  %+@ Timecode t = Timecode $ δt + t
Timecode t  @+% RelTime δt = Timecode $ t + δt
Timecode t  @-% RelTime δt = Timecode $ t - δt
RelTime δ₁ %-% RelTime δ₂ = RelTime $ δ₁ - δ₂
RelTime δ₁ %+% RelTime δ₂ = RelTime $ δ₁ + δ₂
Frequency ν₁ ~-~ Frequency ν₂ = Frequency $ ν₁ - ν₂
Frequency ν₁ ~+~ Frequency ν₂ = Frequency $ ν₁ + ν₂

(%*), (%/) :: Real i => RelTime -> i -> RelTime
(*%) :: Real i => i -> RelTime -> RelTime
(%/%) :: RealFrac q => RelTime -> RelTime -> q
RelTime δt %* n = RelTime $ δt * realToFrac n
RelTime δt %/ n = RelTime $ δt / realToFrac n
n *% RelTime δt = RelTime $ realToFrac n * δt
RelTime δ₁ %/% RelTime δ₂ = realToFrac $ δ₁ / δ₂

{-# SPECIALISE (%*) :: RelTime -> Double -> RelTime #-}
{-# SPECIALISE (%/) :: RelTime -> Double -> RelTime #-}
{-# SPECIALISE (%*) :: RelTime -> Int -> RelTime #-}
{-# SPECIALISE (*%) :: Double -> RelTime -> RelTime #-}
{-# SPECIALISE (*%) :: Int -> RelTime -> RelTime #-}
{-# SPECIALISE (%/%) :: RelTime -> RelTime -> Double #-}

(~*) :: Real i => Frequency -> i -> Frequency
(~/) :: Real i => Frequency -> i -> Frequency
(*~) :: Real i => i -> Frequency -> Frequency
(~/~) :: RealFrac q => Frequency -> Frequency -> q
Frequency ν ~* n = Frequency $ ν * realToFrac n
Frequency ν ~/ n = Frequency $ ν / realToFrac n
n *~ Frequency ν = Frequency $ realToFrac n * ν
Frequency ν₁ ~/~ Frequency ν₂ = realToFrac $ ν₁ / ν₂

{-# SPECIALISE (~*) :: Frequency -> Double -> Frequency #-}
{-# SPECIALISE (~/) :: Frequency -> Double -> Frequency #-}
{-# SPECIALISE (~*) :: Frequency -> Int -> Frequency #-}
{-# SPECIALISE (~/) :: Frequency -> Int -> Frequency #-}
{-# SPECIALISE (*~) :: Double -> Frequency -> Frequency #-}
{-# SPECIALISE (*~) :: Int -> Frequency -> Frequency #-}
{-# SPECIALISE (~/~) :: Frequency -> Frequency -> Double #-}

(/%) :: Real i => i -> RelTime -> Frequency
(/~) :: Real i => i -> Frequency -> RelTime
(~*%) :: RealFrac q => Frequency -> RelTime -> q
(%*~) :: RealFrac q => RelTime -> Frequency -> q
n /% RelTime τ = Frequency $ realToFrac n / τ
n /~ Frequency ν = RelTime $ realToFrac n / ν
Frequency ν ~*% RelTime τ = realToFrac $ ν * τ
RelTime τ %*~ Frequency ν = realToFrac $ τ * ν

{-# SPECIALISE (/%) :: Double -> RelTime -> Frequency #-}
{-# SPECIALISE (/%) :: Int -> RelTime -> Frequency #-}
{-# SPECIALISE (/~) :: Double -> Frequency -> RelTime #-}
{-# SPECIALISE (/~) :: Int -> Frequency -> RelTime #-}
{-# SPECIALISE (~*%) :: Frequency -> RelTime -> Double #-}
{-# SPECIALISE (%*~) :: RelTime -> Frequency -> Double #-}


-- | 'laterThan' and 'earlierThan' are basically @\>@ and @\<@ for 'Timecode'.
-- ('RelTime' has an actual @Ord@ instance.)
infix 4 `laterThan`, `earlierThan`
laterThan, earlierThan :: Timecode -> Timecode -> Bool
Timecode t₁ `laterThan` Timecode t₂ = t₁ > t₂
Timecode t₁ `earlierThan` Timecode t₂ = t₁ < t₂

