module Media.Timed.Audio.Example( module Media.Timed.Audio
                           , module Media.Timed.Timecode.Arith
                           , sineTest, sineDecaying, simpleDecayTone
                           ) where

import Media.Timed.Timecode.Arith
import Media.Timed.Audio

sineTest :: Frequency -> Audio
sineTest ν = audioFn f
    where f t = sin $ 2*pi * (ν ~*% (t @-@ timeZero))
--    = Timeline $ \δt t₀ ->  [ AudioChunk δt $ chunkGen tᵢ | tᵢ <- iterate(%+% δt) (t₀ @-@ timeZero) ]
--   where chunkGen tᵢ (PCM δs) = [ realToFrac . sin $ 2*pi * (ν~*%t) | t <- iterate(%+% δs) tᵢ ]

sineDecaying :: Frequency -> Frequency -> Audio
sineDecaying μ ν = audioFn f
    where f t = sin (2*pi * (ν ~*% t')) * exp (negate $ μ ~*% t')
           where t' = t @-@ timeZero

simpleDecayTone :: Float -> Float -> Frequency -> Frequency -> Audio
simpleDecayTone harmCont evenHarms μ ν = audioFn f
  where f t 
          | t `laterThan` timeZero = wfn t' ω -- -- * exp (μ ~*% t' / 2)
          | otherwise = 0
          where ω = 2*pi * (ν ~*% t')
                t' = t @-@ timeZero
        wfn t' ω = dst $ (sin ω + sin (2*ω) * evenHarms) * exp (negate $ μ ~*% t') * harmCont
          where dst a = a / (1 + a^2)