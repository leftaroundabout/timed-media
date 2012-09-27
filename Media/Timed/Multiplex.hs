-- |
-- Module      : Media.Timed.Multiplex
-- Copyright   : (c) Justus Sagemüller 2012
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemuej $ smail.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
-- 

{-# LANGUAGE MultiParamTypeClasses  #-}
-- {-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FlexibleInstances      #-}


module Media.Timed.Multiplex ( module Media.Timed.Timeline
                             , Multiplex  (..)
                             , MuxMixable (..)
                             ) where


import Media.Timed.Timeline


class Multiplex ml where
  type MultiplexParams ml :: *
  type MultiplexChunks ml :: *
  multiplex :: ml -> Timeline (MultiplexParams ml) (MultiplexChunks ml)
--  deMultiplex :: Timeline (MultiplexParams ml) (MultiplexChunks ml) -> ml


class MuxMixable params param chunks chunk where
  muxMix :: Timeline params chunks
         -> Timeline param  chunk

