module MMedia.Timeline ( Timecode
                       , RelTime
                       , (-@), (@+), (@-), (-%), (+%), (+@), (/%), (%*), (*%)
                       , laterThan, earlierThan
                       , Timeline(Timeline)
                       , class Chunky
                       , appendAt
                       , CrossRatio
                       , class Crossable
                       , crossOvr
                       ) where



newtype Timecode = Timecode { getTimecode :: Double }
newtype RelTime  = RelTime  { getRelTime  :: Double }


-- simple timecode arithmetics:

infix 6 -@               -- there is always an absolute time at the side of an @
infixl 6 @+, @-, -%, +%  -- and always a relative time at the side of a %
infixr 6 +@
infix 7 /%
infixl 7 %*
infixr 7 *%

(-@) :: Timecode -> Timecode -> RelTime
(+@) :: RelTime -> Timecode -> Timecode
(@+), (@-) :: Timecode -> RelTime -> Timecode
(-%), (+%) :: RelTime -> RelTime -> RelTime

Timecode t₁ -@ Timecode t₂ = RelTime $ t₁ - t₂
RelTime δt  +@ Timecode t = Timecode $ δt + t
Timecode t  @+ RelTime δt = Timecode $ t + δt
Timecode t  @- RelTime δt = Timecode $ t - δt
RelTime δ₁ -% RelTime δ₂ = RelTime $ δ₁ - δ₂
RelTime δ₁ +% RelTime δ₂ = RelTime $ δ₁ + δ₂

(%*) :: Real i => RelTime -> i -> RelTime
(*%) :: Real i => i -> RelTime -> RelTime
(/%) :: RealFrac q => RelTime -> RelTime -> q
RelTime δt %* n = RelTime $ δt * realToFrac n
n *% RelTime δt = RelTime $ realToFrac n * δt
RelTime δ₁ /% RelTime δ₂ = realToFrac $ δ₁ / δ₂

infix 4 laterThan, earlierThan
laterThan, earlierThan :: Timecode -> Timecode -> Bool
Timecode t₁ `laterThan` Timecode t₂ = t₁ > t₂
Timecode t₁ `earlierThan` Timecode t₂ = t₁ < t₂

oneSecond :: RelTime
oneSecond = RelTime 1

timeZero :: Timecode
timeZero = Timecode 0


newtype Timeline chnk = Timeline { runTimeline :: RelTime   -- desired length of the chunks
                                               -> Timecode  -- starting time for first chunk
                                               -> [chnk] }

instance Functor Timeline where
  fmap f (Timeline line) = Timeline $ \δt -> map f . line δt


class Chunky chnk where
  
     -- (splitOvr tₓ c₁ c₂) is a chunk ≡c₁ for t < tₓ and ≡c₂ for t ≥ tₓ
  switchOvr :: RelTime -> chnk -> chnk -> chnk
  
  -- rechunk :: RelTime -> [chnk] -> [chnk]




switchAt :: Chunky c => Timecode -> Timeline c -> Timeline c -> Timeline c
switchAt tₓ (Timeline line₁) (Timeline line₂) = Timeline result
 where result δt t₀
         | t₀ `laterThan` tₓ   = line₂ t₀ δt
         | otherwise           = lhs ++ switchOvr (tₓ -@ tₓ') spl₁ spl₂ : rhs
    where (lhs, (spl₁:_)) = splitAt lhsChunks $ line₁ δt t₀
          (spl₂ : rhs) = line₂ δt tₓ'
          lhsChunks = floor $ (tₓ -@ t₀)/δt
          tₓ' = t₀ @+ lhsChunks *% δt

delay :: RelTime -> Timeline c -> Timeline c
delay tΔ (Timeline line) = Timeline $ \δt t₀ -> line δt (t₀ -@ tΔ)


timeChain :: [(Timecode, Timeline c)] -> Timeline c
timeChain [(_, line)] = line
timeChain ((t₁, line₁) : remain)
     = glue $ map ( \(tᵣ, line) -> (tᵣ, delay (tᵣ -@ t₁) line) ) remain
 where glue = foldl (\acc (tᵣ, line) -> switchAt tᵣ acc line) line



type CrossRatio = Float

lhsOnly, rhsOnly :: CrossRatio
lhsOnly = -1, rhsOnly = 1

class Chunky chnk => Crossable chnk where
  
  crossOvr :: (RelTime -> CrossRatio) -> chnk -> chnk -> chnk

type XOverForm = CrossRatio -> CrossRatio   -- should be a homeomorphism [-1,1] -> [-1,1]


xOverAt :: Crossable c
    => Timecode     -- central time of crossover
    -> RelTime      -- time the x-over reaches into each of the parts (from central time, so the total x-over duration is twice this value)
    -> XOverForm    -- morphing curve to use
    -> Timeline c -> Timeline c -> Timeline c
xOverAt tₓ reachₓ fadeFn (Timeline line₁) (Timeline line₂) = Timeline result
 where result δt t₀
         | t₀ `laterThan` tₓ @+ reachₓ  = line₂ δt t₀
         | otherwise                   = lhs ++ faderange ++ rhs
    where (lhs, lhstail) = splitAt lhsChunks $ line₁ δt t₀
          xpl₁ = take fadeChunks lhstail
          (xpl₂, rhs) = splitAt fadeChunks $ line₂ δt (tFadeStart -@ tₓ)
          faderange = zipWith3 (\n -> let tᵤ = (tFadeStart @+ n *% δt) -@ tₓ
                                       in crossOvr (fadeFn . (/% reachₓ) . (+% tᵤ))
                               ) [0..] xpl₁ xpl₂

          lhsChunks = floor $ ((tₓ @- reachₓ) -@ t₀)/δt
          fadeChunks = ceiling( ((tₓ @+ reachₓ) -@ t₀)/δt ) - lhsChunks
          tFadeStart = t₀ @+ lhsChunks *% δt



