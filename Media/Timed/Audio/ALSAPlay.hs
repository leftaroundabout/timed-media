-- |
-- Module      : Media.Timed.Audio.ALSAPlay
-- Copyright   : (c) Justus Sagemüller 2012
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemuej $ smail.uni-koeln.de
-- Stability   : experimental
-- Portability : non-portable (requires Linux/ALSA)
-- 
-- 

module Media.Timed.Audio.ALSAPlay( module Media.Timed.Audio
                            , aplaySimple
                            , aplaySimpleLogged
                            ) where

import Media.Timed.Timecode.Arith
import Media.Timed.Audio

import System.IO
import System.Process

import qualified Data.ByteString.Lazy as BL
import Data.Binary
import Data.Binary.Put
import Data.Int
import Unsafe.Coerce

import qualified Data.Vector.Storable as VU

import Control.Monad


iChunkRate  = (1/10) *% oneSecond            -- chunk length
iSampleRate = (1/44100) *% oneSecond    -- CDA-type sampling


-- | Play a mono audio stream by piping it to aplay
--   in 16 Bit, 44.1 kHz, in chunks of length 100 ms.
aplaySimple :: Timecode -> Audio -> IO ()
aplaySimple = pipePlay_CDQ "aplay -f cd >/dev/null 2>/dev/null" -- tee aplyspleoutp.wvcd | 


-- | Like aplaySimple, but also writes the output to a binary log file.
aplaySimpleLogged :: FilePath -> Timecode -> Audio -> IO ()
aplaySimpleLogged logFile
   = pipePlay_CDQ $ "tee "++logFile++ "| aplay -f cd >/dev/null 2>/dev/null"


pipePlay_CDQ :: String -> Timecode -> Audio -> IO ()
pipePlay_CDQ pipeCmp t₀ aud = do
    (aplaySTDIn, _, _, aplayProc) <- runInteractiveCommand pipeCmp
    hSetBuffering aplaySTDIn $ BlockBuffering (Just 1024)

    forM_ ( staticRenderTimeline iChunkRate
                                 t₀        -- start time
                                 (PCM iSampleRate)
                                 aud                     )  (
       \c -> case renderAudioChunk c of
         Just (SampleCnt sl chunk) -> BL.hPut aplaySTDIn . runPut
           $ VU.forM_ chunk (
               \sample -> do
                let outSple = round . max (-32768) . min 32767 $ sample * 32768 :: Int16
                putWord16le $ unsafeCoerce outSple
                putWord16le $ unsafeCoerce outSple
              )
         Nothing -> BL.hPut aplaySTDIn
            $ BL.replicate (2 * 2 * round(iChunkRate%/%iSampleRate)) 0
--        hFlush aplaySTDIn
     )


