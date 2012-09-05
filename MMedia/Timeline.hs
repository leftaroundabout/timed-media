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
-- both forwards and \"backwards\" in time, though of course there will, in
-- general, not for all @t@ be anything meaningful to read). Access is by
-- specifying a starting time; this yields all the history from that point on.
-- The information itself is stored in strictly-evaluated chunks (which have
-- therefore good performance potential). The length of these chunks can be
-- specified when evaluating the Timeline object, so you can make latency/performance
-- tradeoffs as fits for each application.
-- Furthermore, some custom parameters (such as audio sample rate or video frame
-- rate and resolution) may be specified for the rendering of each chunk, so
-- the quality of rendering may be adjusted on-the-fly to fit CPU availability
-- (mainly intended for computionally expensive HD-video editing).

module MMedia.Timeline ( -- * Imports
                         module MMedia.Timecode
                       , -- * Timeline data type
                         Timeline(Timeline, runTimeline), staticRenderTimeline
                       , -- * Proxy types used to implement rendering of timelines
                         TimeRendering(TimeRendering, runTimeRender)
                       , TimePresentation(TimePresentation, getTimePresentation, nPreloaded)
                       , -- * General, chunk-agnostic operations
                         delay
                       , -- * Chunk-based editing of timelines
                         cmap
                       , -- * Classes of timeline chunks, to support various common operations
                         -- ** \"Cuttable\"
                         Chunky(switchOvr)
                       , switchAt, timeChain
                       , -- ** Chunks that allow \"crossover\" transitions
                         CrossRatio, lhsOnly, rhsOnly
                       , Crossable(crossOvr)
                       , XOverForm, xOverAt
                       , -- ** Chunks that can be mixed commutatively, like audio
                         Mixable(mixChunks)
                       , mix
                       , -- ** Chunks that can be gain-adjusted or α-scaled
                         Gain, Gainable(gainChunk), gain
                       ) where



import MMedia.Timecode
import MMedia.Timecode.Arith

import Data.List


-- | 'TimeRendering' represents a \"time-thread\": evaluating each 'runTimeRender'
-- yields a time-chunk and a 'TimeRendering' that can be used to generate the next chunk.
-- it is not possible to skip chunks without rendering them, use a fresh 'Timeline'
-- evaluation to to such a thing.

newtype TimeRendering chnkGenParams chnk
   = TimeRendering { runTimeRender :: chnkGenParams
                                   -> (chnk, TimeRendering chnkGenParams chnk) }


takenChunksThen :: Int -> TimeRendering cgp chnk -> TimeRendering cgp chnk -> TimeRendering cgp chnk
takenChunksThen n (TimeRendering r1) r2
  | n>0        = TimeRendering $ \ ps -> let (chnk, r1') = r1 ps
                                         in  (chnk, takenChunksThen (n-1) r1' r2)
  | otherwise  = r2


takenChunksThenOvrlap :: Int -> Int -> (Int->chnk->chnk->chnk)
                       -> TimeRendering cgp chnk -> TimeRendering cgp chnk
                       -> TimeRendering cgp chnk
takenChunksThenOvrlap nL nOvrlap combine (TimeRendering r1) r2
  | nL>0
      = TimeRendering $ \ ps -> let (chnk, r1') = r1 ps
                                in  (chnk, takenChunksThenOvrlap (nL-1)
                                               nOvrlap combine r1' r2   )
  | nOvrlap>0
      = TimeRendering $ \ ps -> let (chnk1, r1') = r1 ps
                                    (chnk2, r2') = runTimeRender r2 ps
                                in  ( combine (-nL) chnk1 chnk2
                                    , takenChunksThenOvrlap (nL-1)
                                               (nOvrlap-1) combine r1' r2' )
  | otherwise  = r2


takenChunksModify1Then :: Int -> TimeRendering cgp chnk
                 -> (chnk->chnk) -> TimeRendering cgp chnk
                  -> TimeRendering cgp chnk
takenChunksModify1Then n (TimeRendering r1) mdfy r2
  = TimeRendering $ \ ps
        -> let (chnk, r1') = r1 ps
           in  if n>0 then (chnk, takenChunksModify1Then (n-1) r1' mdfy r2)
                      else (mdfy chnk, r2)


dropChunks :: Int -> TimeRendering cgp chnk -> TimeRendering cgp chnk
dropChunks n r = TimeRendering $ \ ps -> drop' n r ps
 where drop' n r ps = let (chunk, r') = runTimeRender r ps
                      in  if n>0 then drop' (n-1) r' ps
                                 else (chunk, r')


data TimePresentation chnkGenParams chnk
   = TimePresentation { getTimePresentation :: TimeRendering chnkGenParams chnk
                      , nPreloaded :: Int  -- ^ Number of chunks before the specified replay-starting time of the timeline
                      }

newtype Timeline chnkGenParams chnk
      = Timeline {   -- | desired length of the chunks -> starting time for first chunk -> minimum preload -> rendered timeline
                   runTimeline :: RelTime
                               -> Timecode
                               -> RelTime
                               -> TimePresentation chnkGenParams chnk }

staticRenderTimeline :: RelTime                     -- ^ Chunk length
                     -> Timecode                    -- ^ Start pos
                     -> chnkGenParams               -- ^ Chunk-rendering parameters
                     -> Timeline chnkGenParams chnk -- ^ Timeline to render
                     -> [chnk]                      -- ^ Infinite list of rendered chunks
staticRenderTimeline δt t₀ ps (Timeline line) = drop nPre $ unfold rend
 where (TimePresentation rend nPre) = line δt t₀ noTime
       unfold (TimeRendering r) = chunk : unfold rest
        where (chunk,rest) = r ps



-- | 'cmap' is equivalent to @fmap@, mapping timeline chunks.
-- 
-- Timeline should actually be a kind of functor, but not so much for chunks as for samples / frames.
-- Something like that might be doable with type families, but not for the Prelude Functor type class.
-- 
-- @
--   instance Functor Timeline where
--     fmap = cmap  ??
-- @

cmap :: (c->c') -> Timeline p c -> Timeline p c'
cmap f (Timeline line) = Timeline $ \δt t₀ ps
           -> let (TimePresentation (TimeRendering rend) nPreload) = line δt t₀ ps
              in  TimePresentation (TimeRendering $ rmap rend) nPreload
 where rmap rend cgp = let (chnk, TimeRendering cont) = rend cgp
                       in  (f chnk, TimeRendering $ rmap cont)




class Chunky chnk where
  
     -- (splitOvr tx c₁ c₂) is a chunk ≡c₁ for t < tx and ≡c₂ for t ≥ tx
  switchOvr :: RelTime -> chnk -> chnk -> chnk
  
  -- rechunk :: RelTime -> [chnk] -> [chnk]



switchAt :: Chunky c => Timecode      -- ^ \"Switching time\" /tₓ/
                     -> Timeline p c  -- ^ \"Early\" timeline ℓ₁
                     -> Timeline p c  -- ^ \"Early\" timeline ℓ₂
                     -> Timeline p c  -- ^ Timeline that is equivalent to ℓ₁ for /t/ \< /tₓ/ and equivalent to ℓ₂ for /t/ \> /tₓ/.
switchAt tx (Timeline line₁) (Timeline line₂) = Timeline result
 where result δt t₀ tpre
         | t₀@-%tpre `laterThan` tx  = line₂ δt t₀ tpre
         | otherwise                 = TimePresentation switched nPreload
        where switched = takenChunksThenOvrlap (lhsChunks+nPreload) 1
                                (const $ switchOvr (tx @-@ tx')) llhs rrhs
              TimePresentation llhs nPreload = line₁ δt t₀   tpre
              TimePresentation rhs nPreloadr = line₂ δt tx'' tprer
              rrhs = dropChunks (nPreloadr - 1) rhs
              lhsChunks = floor $ (tx @-@ t₀) %/% δt
              tx' = t₀ @+% lhsChunks *% δt  -- start time of the overlap-chunk
              tx'' = tx' @+% δt             -- end time   〃  〃      〃
              tprer = max ε $ tx'' @-@ tx
               where ε = δt %/ 1000



delay :: RelTime -> Timeline p c -> Timeline p c
delay tΔ (Timeline line) = Timeline $ \δt t₀ -> line δt (t₀ @-% tΔ)


timeChain :: Chunky c => [(Timecode, Timeline p c)] -> Timeline p c
timeChain [(_, line)] = line
timeChain (l₁@(t₁, line₁) : remain)
     = glue $ l₁ : map ( \(tᵣ, line) -> (tᵣ, delay (tᵣ @-@ t₁) line) ) remain
 where glue = snd . foldr1 (\(tᵣ, line) (tᵤ, acc) -> (tᵣ, switchAt tᵤ line acc))



type CrossRatio = Float

lhsOnly, rhsOnly :: CrossRatio
lhsOnly = -1; rhsOnly = 1

class Chunky chnk => Crossable chnk where
  
  crossOvr :: (RelTime -> CrossRatio) -> chnk -> chnk -> chnk

-- | should be a homeomorphism [-1,1] -> [-1,1]
type XOverForm = CrossRatio -> CrossRatio


xOverAt :: Crossable c
    => Timecode     -- central time of crossover
    -> RelTime      -- time the x-over reaches into each of the parts (from central time, so the total x-over duration is twice this value)
    -> XOverForm    -- morphing curve to use
    -> Timeline p c -> Timeline p c -> Timeline p c
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

  mixChunks :: [chnk] -> chnk -- ^ All chunks in the list are guaranteed to have been generated with the same parameters.


mix :: Mixable c => [Timeline p c] -> Timeline p c
mix lns = Timeline result
 where result δt t₀ tpre = performMix [ runTimeline tl δt t₀ tpre | tl <- lns ]
       performMix lineLs = TimePresentation (mixdown renders) minPreN
         where minPreN = minimum $ map nPreloaded lineLs
               renders = map ( \(TimePresentation ren nPre)
                                  -> dropChunks (nPre-minPreN) ren ) lineLs
               mixdown rs = TimeRendering $ \ps
                               -> (\(chunks, conts) -> (mixChunks chunks, mixdown conts))
                                      . unzip $ map(($ps) . runTimeRender) rs


type Gain = Float

class Gainable chnk where
  gainChunk :: Gain -> chnk -> chnk

gain :: Gainable c => Gain -> Timeline p c -> Timeline p c
gain = cmap . gainChunk