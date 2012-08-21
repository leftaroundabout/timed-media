module MMedia.Audio.ALSAPlay( module MMedia.Audio
                            , aplaySimple
                            ) where

import MMedia.Timecode.Arith
import MMedia.Audio

import System.IO
import System.Process

import Data.ByteString.Lazy
import Data.Binary
import Data.Binary.Put
import Data.Int
import Unsafe.Coerce

import Control.Monad

   -- Play a mono audio stream by piping it to aplay in 16 Bit, 44.1 kHz
aplaySimple :: Timecode -> Audio -> IO ()
aplaySimple t₀ aud = do
    (aplaySTDIn, _, _, aplayProc) <- runInteractiveCommand "tee aplyspleoutp.wvcd | aplay -f cd >/dev/null 2>/dev/null"
    hSetBuffering aplaySTDIn $ BlockBuffering (Just 1024)
    forM_ (runTimeline aud ((1/10) *% oneSecond) t₀) (\chunk -> do
--        System.IO.putStrLn "lala"
       hPut aplaySTDIn . runPut
         $ forM_ (renderAudioChunk chunk (PCM $ (1/44100) *% oneSecond)) (
             \sample -> do
              let outSple = round . max (-32768) . min 32767 $ sample * 32768 :: Int16
              putWord16le $ unsafeCoerce outSple
              putWord16le $ unsafeCoerce outSple
            )
--        hFlush aplaySTDIn
     )
