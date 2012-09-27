-- |
-- Module      : Media.Timed.Multiplex.HList
-- Copyright   : (c) Justus Sagemüller 2012
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemuej $ smail.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
-- 

{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FlexibleInstances      #-}


module Media.Timed.Multiplex.HList( module Media.Timed.Multiplex
                                  ) where


import Media.Timed.Multiplex
import Media.Timed.Timecode.Arith

import Data.HList.HListPrelude




instance Multiplex HNil where
  type MultiplexParams HNil = HNil
  type MultiplexChunks HNil = HNil
  multiplex HNil = Timeline $ \δt t₀ tpre
                     -> let nilRender = TimeRendering $ \HNil -> (HNil, nilRender)
                            nPre = ceiling $ tpre %/% δt
                        in  TimePresentation nilRender nPre

instance (Multiplex l) => Multiplex (HCons (Timeline p c) l) where
  type MultiplexParams (HCons (Timeline p c) l) = HCons p (MultiplexParams l)
  type MultiplexChunks (HCons (Timeline p c) l) = HCons c (MultiplexChunks l)
  multiplex (HCons (Timeline headLine) mp) = Timeline cons'dLine
   where cons'dLine δt t₀ tpre
                 = TimePresentation
                      (TimeRendering $ consRender trimdHeadRender trimdTailRender)
                      nPre
          where consRender     ( TimeRendering hRender ) ( TimeRendering tRender )
                           ( HCons             hParam                    tParams )
                         = ( HCons             hChunk                    tChunks
                           , TimeRendering $
                                 consRender    hCont                     tCont   )
                 where (hChunk, hCont) = hRender hParam
                       (tChunks, tCont) = tRender tParams

                trimdHeadRender = dropChunks(max 0 $ nPreH - nPre) headRender
                trimdTailRender = dropChunks(max 0 $ nPreT - nPre) tailRender

                nPre = min nPreH nPreT

                (TimePresentation headRender nPreH) = headLine δt t₀ tpre

                (TimePresentation tailRender nPreT) = tailLine δt t₀ tpre
                (Timeline tailLine) = multiplex mp



class (HList2List l e) => HomogenHList l e where
  hRepeat :: e -> l

instance HomogenHList HNil e where
  hRepeat _ = HNil

instance (HomogenHList l e) => HomogenHList (HCons e l) e where
  hRepeat e = HCons e $ hRepeat e


instance ( HomogenHList params param
         , HomogenHList chunks chunk
         , Mixable chunk
         ) => MuxMixable params param chunks chunk where
  muxMix = cmap' (\p -> (hRepeat p, mixChunks . hList2List))

