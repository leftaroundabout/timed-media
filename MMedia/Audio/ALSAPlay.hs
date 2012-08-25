-- |
-- Module      : MMedia.Audio.ALSAPlay
-- Copyright   : (c) Justus Sagemüller 2012
-- License     : GPL v3
-- 
-- Maintainer  : sagemuej@smail.uni-koeln.de
-- Stability   : experimental
-- Portability : non-portable (requires Linux/ALSA)
-- 
-- 

module MMedia.Audio.ALSAPlay( module MMedia.Audio
                            , aplaySimple
                            ) where

import MMedia.Timecode.Arith
import MMedia.Audio

import System.IO
import System.Process

import qualified Data.ByteString.Lazy as BL
import Data.Binary
import Data.Binary.Put
import Data.Int
import Unsafe.Coerce

import qualified Data.Vector.Storable as VU

import Control.Monad

-- | Play a mono audio stream by piping it to aplay
--   in 16 Bit, 44.1 kHz, in chunks of length 100 ms.
aplaySimple :: Timecode -> Audio -> IO ()
aplaySimple t₀ aud = do
    (aplaySTDIn, _, _, aplayProc) <- runInteractiveCommand "aplay -f cd >/dev/null 2>/dev/null" -- tee aplyspleoutp.wvcd | 
    hSetBuffering aplaySTDIn $ BlockBuffering (Just 1024)

    forM_ (timeRenderedChunks $ runTimeline aud
                  ((1/10) *% oneSecond)   -- chunk length
                  t₀                      -- start
                  noTime                  -- preload
                                                  ) (
       \c -> case renderAudioChunk c of
         Just chunk -> BL.hPut aplaySTDIn . runPut
           $ VU.forM_ (chunk (PCM $ (1/44100) *% oneSecond)) (
               \sample -> do
                let outSple = round . max (-32768) . min 32767 $ sample * 32768 :: Int16
                putWord16le $ unsafeCoerce outSple
                putWord16le $ unsafeCoerce outSple
              )
         Nothing -> BL.hPut aplaySTDIn $ BL.replicate (2 * 2 * 44100 `div` 10) 0
--        hFlush aplaySTDIn
     )
