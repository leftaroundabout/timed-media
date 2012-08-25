-- |
-- Module      : MMedia.Timecode.Arith
-- Copyright   : (c) Justus Sagemüller 2012
-- License     : GPL v3
-- 
-- Maintainer  : sagemuej@smail.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
-- 
module MMedia.Timecode.Arith( module MMedia.Timecode 
                            , (@-@), (@+%), (@-%), (%+%), (%-%)
                            , (~+~), (~-~), (%+@), (/%), (%/%), (/~), (~/~)
                            , (~*%), (%*~), (%*), (~*), (*%), (*~)
                            , earlierThan, laterThan
                            )where

import MMedia.Timecode
import MMedia.Timecode.Internal

infix 6 @-@                  -- there is always an absolute time at the side of an @,
infixl 6 @+%, @-%            -- always a relative time at the side of a %
infixl 6 %+%, %-%, ~+~, ~-~  -- always a frequency at the side of a ~
infixr 6 %+@
infix 7 /%, %/%, /~, ~/~, ~*%, %*~
infixl 7 %*, ~*
infixr 7 *%, *~

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

(%*) :: Real i => RelTime -> i -> RelTime
(*%) :: Real i => i -> RelTime -> RelTime
(%/%) :: RealFrac q => RelTime -> RelTime -> q
RelTime δt %* n = RelTime $ δt * realToFrac n
n *% RelTime δt = RelTime $ realToFrac n * δt
RelTime δ₁ %/% RelTime δ₂ = realToFrac $ δ₁ / δ₂

(~*) :: Real i => Frequency -> i -> Frequency
(*~) :: Real i => i -> Frequency -> Frequency
(~/~) :: RealFrac q => Frequency -> Frequency -> q
Frequency ν ~* n = Frequency $ ν * realToFrac n
n *~ Frequency ν = Frequency $ realToFrac n * ν
Frequency ν₁ ~/~ Frequency ν₂ = realToFrac $ ν₁ / ν₂

(/%) :: Real i => i -> RelTime -> Frequency
(/~) :: Real i => i -> Frequency -> RelTime
(~*%) :: RealFrac q => Frequency -> RelTime -> q
(%*~) :: RealFrac q => RelTime -> Frequency -> q
n /% RelTime τ = Frequency $ realToFrac n / τ
n /~ Frequency ν = RelTime $ realToFrac n / ν
Frequency ν ~*% RelTime τ = realToFrac $ ν * τ
RelTime τ %*~ Frequency ν = realToFrac $ τ * ν


infix 4 `laterThan`, `earlierThan`
laterThan, earlierThan :: Timecode -> Timecode -> Bool
Timecode t₁ `laterThan` Timecode t₂ = t₁ > t₂
Timecode t₁ `earlierThan` Timecode t₂ = t₁ < t₂


instance Eq RelTime where
  RelTime δ₁ == RelTime δ₂ = δ₁ == δ₂
  RelTime δ₁ /= RelTime δ₂ = δ₁ /= δ₂

instance Ord RelTime where
  RelTime δ₁ < RelTime δ₂ = δ₁ < δ₂
  RelTime δ₁ > RelTime δ₂ = δ₁ > δ₂
  RelTime δ₁ <= RelTime δ₂ = δ₁ <= δ₂
  RelTime δ₁ >= RelTime δ₂ = δ₁ >= δ₂

