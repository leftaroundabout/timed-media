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

{-# LANGUAGE FlexibleInstances    #-}


module Media.Timed.Audio.ALSAPlay( module Media.Timed.Audio
                            , AlsaPlayable
                            , aplaySimple
                            , aplaySimpleLogged
                            ) where

import Media.Timed.Timecode.Arith
import Media.Timed.Audio
import Media.Timed.Audio.Channels

import System.IO
import System.Process

import qualified Data.ByteString.Lazy as BL
import Data.Binary
import Data.Binary.Put
import Data.Int
import Unsafe.Coerce

import Data.HList.HListPrelude

import qualified Data.Vector.Storable as VU

import Control.Monad




iChunkRate  = (1/10) *% oneSecond            -- chunk length
iSampleRate = (1/44100) *% oneSecond    -- CDA-type sampling
cdSampling = PCM iSampleRate


-- | Play an audio stream by piping it to aplay
--   in 16 Bit, 44.1 kHz, in chunks of length 100 ms.
aplaySimple :: AlsaPlayable audio
             => Timecode -> audio -> IO ()
aplaySimple = pipePlay_CDQ "aplay -f cd >/dev/null 2>/dev/null" -- tee aplyspleoutp.wvcd | 


-- | Like aplaySimple, but also writes the output to a binary log file.
aplaySimpleLogged :: AlsaPlayable audio
             => FilePath -> Timecode -> audio -> IO ()
aplaySimpleLogged logFile
   = pipePlay_CDQ $ "tee "++logFile++ "| aplay -f cd >/dev/null 2>/dev/null"




class AlsaPlayable audio where
  pipePlay_CDQ :: String -> Timecode -> audio -> IO ()



instance AlsaPlayable Audio where
  pipePlay_CDQ = pipePlay_CDQ' cdSampling chunkPut
   where chunkPut outPipe c
           = case renderAudioChunk c of
              (Just (SampleCnt sl chunk))
                -> BL.hPut outPipe . runPut
                    $ VU.forM_ chunk (
                        \sample -> do
                         let outSple = samplePrep16 sample
                         putWord16le $ unsafeCoerce outSple
                         putWord16le $ unsafeCoerce outSple
                       )
              Nothing -> BL.hPut outPipe emptyChunkContent

instance AlsaPlayable StereoAudio where
  pipePlay_CDQ = pipePlay_CDQ' (HCons cdSampling (HCons cdSampling HNil)) chunkPut
   where chunkPut outPipe (HCons lc (HCons rc HNil))
            = case (renderAudioChunk lc, renderAudioChunk rc) of
               (Just (SampleCnt sl lChunk), Just (SampleCnt sl' rChunk))
                 -> BL.hPut outPipe . runPut
                   $ VU.zipWithM_ (
                      \lsample rsample -> do
                       putWord16le $ unsafeCoerce (samplePrep16 rsample)
                       putWord16le $ unsafeCoerce (samplePrep16 lsample)
                     ) lChunk rChunk
               (Just (SampleCnt sl lChunk), Nothing)
                 -> BL.hPut outPipe . runPut
                   $ VU.forM_ lChunk (
                      \sample -> do
                       putWord16le 0
                       putWord16le $ unsafeCoerce (samplePrep16 sample)
                     )
               (Nothing, Just (SampleCnt sl rChunk))
                 -> BL.hPut outPipe . runPut
                   $ VU.forM_ rChunk (
                      \sample -> do
                       putWord16le $ unsafeCoerce (samplePrep16 sample)
                       putWord16le 0
                     )
               (Nothing, Nothing) -> BL.hPut outPipe emptyChunkContent




-- pipePlay_CDQ :: (pipe -> audioChunk -> IO())
--          -> String -> Timecode -> audio -> IO ()
pipePlay_CDQ' chunkArg chunkPut pipeCmp t₀ aud = do
    (aplaySTDIn, _, _, aplayProc) <- runInteractiveCommand pipeCmp
    hSetBuffering aplaySTDIn $ BlockBuffering (Just 1024)

    forM_ ( staticRenderTimeline iChunkRate
                                 t₀        -- start time
                                 chunkArg
                                 aud                     )
       (chunkPut aplaySTDIn)
--        hFlush aplaySTDIn



samplePrep16 :: AudioSample -> Int16
samplePrep16 = round . max (-32768) . min 32767 . (*32768)


emptyChunkContent = BL.replicate (2 * 2 * round(iChunkRate%/%iSampleRate)) 0