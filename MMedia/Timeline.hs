-- |
-- Module      : MMedia.Timeline
-- Copyright   : (c) Justus Sagemüller 2012
-- License     : GPL v3
-- 
-- Maintainer  : sagemuej@smail.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
-- 

module MMedia.Timeline ( module MMedia.Timecode
                       , Timeline(Timeline), runTimeline
                       , Chunky, switchOvr
                       , switchAt, delay, timeChain
                       , CrossRatio, lhsOnly, rhsOnly
                       , Crossable, crossOvr
                       , xOverAt
                       , Mixable, mixChunks
                       , mix
                       ) where



import MMedia.Timecode
import MMedia.Timecode.Arith

import Data.List


newtype Timeline chnk = Timeline { runTimeline :: RelTime   -- desired length of the chunks
                                               -> Timecode  -- starting time for first chunk
                                               -> [chnk] }

instance Functor Timeline where
  fmap f (Timeline line) = Timeline $ \δt -> map f . line δt


class Chunky chnk where
  
     -- (splitOvr tx c₁ c₂) is a chunk ≡c₁ for t < tx and ≡c₂ for t ≥ tx
  switchOvr :: RelTime -> chnk -> chnk -> chnk
  
  -- rechunk :: RelTime -> [chnk] -> [chnk]




switchAt :: Chunky c => Timecode -> Timeline c -> Timeline c -> Timeline c
switchAt tx (Timeline line₁) (Timeline line₂) = Timeline result
 where result δt t₀
         | t₀ `laterThan` tx   = line₂ δt t₀
         | otherwise           = lhs ++ [switchOvr (tx @-@ tx') spl₁ spl₂] ++ rhs
        where (lhs, (spl₁:_)) = splitAt lhsChunks $ line₁ δt t₀
              (spl₂ : rhs) = line₂ δt tx'
              lhsChunks = floor $ (tx @-@ t₀) %/% δt
              tx' = t₀ @+% lhsChunks *% δt

delay :: RelTime -> Timeline c -> Timeline c
delay tΔ (Timeline line) = Timeline $ \δt t₀ -> line δt (t₀ @-% tΔ)


timeChain :: Chunky c => [(Timecode, Timeline c)] -> Timeline c
timeChain [(_, line)] = line
timeChain (l₁@(t₁, line₁) : remain)
     = glue . reverse
         $ l₁ : map ( \(tᵣ, line) -> (tᵣ, delay (tᵣ @-@ t₁) line) ) remain
 where glue (l:ls) = snd
         $ foldl (\(tᵤ, acc) (tᵣ, line) -> (tᵣ, switchAt tᵤ line acc)) l ls



type CrossRatio = Float

lhsOnly, rhsOnly :: CrossRatio
lhsOnly = -1; rhsOnly = 1

class Chunky chnk => Crossable chnk where
  
  crossOvr :: (RelTime -> CrossRatio) -> chnk -> chnk -> chnk

type XOverForm = CrossRatio -> CrossRatio   -- should be a homeomorphism [-1,1] -> [-1,1]


xOverAt :: Crossable c
    => Timecode     -- central time of crossover
    -> RelTime      -- time the x-over reaches into each of the parts (from central time, so the total x-over duration is twice this value)
    -> XOverForm    -- morphing curve to use
    -> Timeline c -> Timeline c -> Timeline c
xOverAt tx reach fadeFn (Timeline line₁) (Timeline line₂) = Timeline result
 where result δt t₀
         | t₀ `laterThan` tx @+% reach  = line₂ δt t₀
         | otherwise                    = lhs ++ faderange ++ rhs
        where (lhs, lhstail) = splitAt lhsChunks $ line₁ δt t₀
              xpl₁ = take fadeChunks lhstail
              (xpl₂, rhs) = splitAt fadeChunks $ line₂ δt tFadeStart
              faderange = zipWith3 (\i -> let tᵤ = (tFadeStart @+% i *% δt) @-@ tx
                                           in crossOvr (fadeFn . (%/% reach) . (%+% tᵤ))
                                   ) [0..] xpl₁ xpl₂

              lhsChunks = floor $ ((tx @-% reach) @-@ t₀) %/% δt
              fadeChunks = ceiling( ((tx @+% reach) @-@ t₀) %/% δt ) - lhsChunks
              tFadeStart = t₀ @+% lhsChunks *% δt


class Mixable chnk where

  mixChunks :: [chnk] -> chnk


mix :: Mixable c => [Timeline c] -> Timeline c
mix lns = Timeline result
 where result δt t₀ = map mixChunks $ transpose [ runTimeline tl δt t₀ | tl <- lns ]