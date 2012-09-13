{-# LANGUAGE FlexibleInstances #-}

import Media.Timed.Timeline
import Media.Timed.Timecode.Arith


newtype TlChunkTest a = TlChunkTest [(Timecode,a)]
                            deriving (Eq, Show)

type TimelineTest a = Timeline () (TlChunkTest a)

δs = oneSecond %/ 30

instance (Show a) => Show (TimelineTest a) where
  show = (++", ...]") . init . show
           . take 10 . staticRenderTimeline (oneSecond %/ 10) (timecode(0:0:[0])) ()


timelineTest :: a -> TimelineTest a
timelineTest a = Timeline rt
 where rt δt t₀ tpre = TimePresentation ( chunkGen $ t₀ @-% nPreload *% δt )
                                        nPreload
        where chunkGen tᵢ = TimeRendering $ \()
                    -> ( TlChunkTest [ (tᵢ @+% δs %* i, a)
                                      | i<-[0 .. floor $ δt %/% δs -1 ] ]
                       , chunkGen (tᵢ @+% δt)                         )
              nPreload = ceiling $ tpre %/% δt


instance Chunky (TlChunkTest a) where
  switchOvr t (TlChunkTest ls) (TlChunkTest ls')
    = TlChunkTest $ take nLeft ls ++ drop nLeft ls'
   where nLeft = floor $ t %/% δs

timeChainTest :: [(Double, a)] -> TimelineTest a
timeChainTest = timeChain . map(\(tc,n)->(timecode[tc],timelineTest n))