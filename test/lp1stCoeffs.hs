import Data.Complex

infixl 7 ⋅
(⋅) :: Fractional a => a->a->a
(⋅) = (*)

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
