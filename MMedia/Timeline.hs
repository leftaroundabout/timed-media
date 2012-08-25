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
-- A 'Timeline' represents time-dependent information such as audio or video,
-- that is accessible for any time at all (i.e. infinitely stretched
-- both forwards and \"backwards\" in time, though of course there will in
-- general be nothing meaningful to read for all times). Access is by specifying
-- a starting time; this yields all the history from that point on. The
-- information itself is stored in strictly-evaluated chunks. The length
-- of these chunks can be specified when evaluating the Timeline object,
-- so you can make latency/performance tradeoffs as fits for each application.

module MMedia.Timeline ( module MMedia.Timecode
                       , TimeRendering(TimeRendering), timeRenderedChunks, preloadChunks
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



data TimeRendering chnk = TimeRendering { timeRenderedChunks :: [chnk]
                                        , preloadChunks :: [chnk]  -- in reverse order, but the chunks themselves are forward.
                                        }

newtype Timeline chnk = Timeline {   -- | desired length of the chunks -> starting time for first chunk -> minimum preload -> rendered timeline
                                   runTimeline :: RelTime
                                               -> Timecode
                                               -> RelTime
                                               -> TimeRendering chnk }



-- Timeline should actually be a kind of functor, but not so much for chunks as for samples / frames.
-- instance Functor Timeline where
--   fmap f (Timeline line) = Timeline $ \δt -> map f . line δt



class Chunky chnk where
  
     -- (splitOvr tx c₁ c₂) is a chunk ≡c₁ for t < tx and ≡c₂ for t ≥ tx
  switchOvr :: RelTime -> chnk -> chnk -> chnk
  
  -- rechunk :: RelTime -> [chnk] -> [chnk]




switchAt :: Chunky c => Timecode -> Timeline c -> Timeline c -> Timeline c
switchAt tx (Timeline line₁) (Timeline line₂) = Timeline result
 where result δt t₀ tpre
         | t₀ `laterThan` tx   = line₂ δt t₀ tpre
         | otherwise           = TimeRendering
                                  (lhs ++ [switchOvr (tx @-@ tx') spl₁ spl₂] ++ rhs)
                                  preload
        where (lhs, (spl₁:_)) = splitAt lhsChunks llhs
              TimeRendering llhs preload = line₁ δt t₀ tpre
              TimeRendering rhs (spl₂:_) = line₂ δt (tx'@+%δt) ((tx'@+%δt) @-@ tx)
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
xOverAt = undefined
-- xOverAt tx reach fadeFn (Timeline line₁) (Timeline line₂) = Timeline result
--  where result δt t₀
--          | t₀ `laterThan` tx @+% reach  = line₂ δt t₀
--          | otherwise                    = lhs ++ faderange ++ rhs
--         where (lhs, lhstail) = splitAt lhsChunks $ line₁ δt t₀
--               xpl₁ = take fadeChunks lhstail
--               (xpl₂, rhs) = splitAt fadeChunks $ line₂ δt tFadeStart
--               faderange = zipWith3 (\i -> let tᵤ = (tFadeStart @+% i *% δt) @-@ tx
--                                            in crossOvr (fadeFn . (%/% reach) . (%+% tᵤ))
--                                    ) [0..] xpl₁ xpl₂
-- 
--               lhsChunks = floor $ ((tx @-% reach) @-@ t₀) %/% δt
--               fadeChunks = ceiling( ((tx @+% reach) @-@ t₀) %/% δt ) - lhsChunks
--               tFadeStart = t₀ @+% lhsChunks *% δt



class Mixable chnk where

  mixChunks :: [chnk] -> chnk


mix :: Mixable c => [Timeline c] -> Timeline c
mix lns = Timeline result
 where result δt t₀ tpre = performMix [ runTimeline tl δt t₀ tpre | tl <- lns ]
       performMix lineLs = TimeRendering runsMix presMix
         where (runs,pres) = unzip . map(\(TimeRendering r p) -> (r,p)) $ lineLs
               [runsMix,presMix] = map (map mixChunks . transpose) [runs,pres]