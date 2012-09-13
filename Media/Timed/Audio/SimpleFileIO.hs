{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Media.Timed.Audio.SimpleFileIO
-- Copyright   : (c) Justus Sagemüller 2012
-- License     : GPL v3
-- 
-- Maintainer  : sagemuej@smail.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
-- 
-- Easy handling of audio files, using lazy 'ByteString's. This is not
-- really great in terms of performance (better strict ByteString?), nor
-- does it keep the memory-consumption as low as might be possible by
-- keeping unused data completely unloaded (in fact, the data is loaded
-- fully into memory even when only evaluated the beginning of an audio
-- file, despite the use of @ByteString.Lazy@).
-- For the future, a more sophisticated way should be sought; it
-- should also be more general — load arbitrary codecs, e.g. via (sadly
-- not working in ghc-7.4 at the moment) "hs-ffmpeg" or via a custom
-- FFI loader written in C (that might use libavcodec directly).
-- The main difficulty seems to get such an approach thread-safe,
-- which it certainly should be.

-- strict 'ByteString's: the whole
-- audio file will be stored in memory. This may be good for replay-performance,
-- but obviously rather bad for memory consumption and loading-times.


module Media.Timed.Audio.SimpleFileIO( module Media.Timed.Audio
                                , loadWAVfileChan
                                , FileLoadFailure (..)
                                ) where

import Media.Timed.Audio
import Media.Timed.Timecode.Arith

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8()
import Data.ByteString.Char8()

import Data.Binary
import Data.Binary.Get

import Data.Bits
import Data.Int

import Numeric(showHex)

import qualified Data.Vector.Storable as VU
-- import qualified Data.Vector.Storable.Mutable as VM

import Control.Monad
-- import Control.Monad.ST
import Control.Monad.Trans
import Control.Monad.Error


data FileContents
  = PCMFileContents { pcmFileAttributes :: PCMFileAttributes
                    , fileDataLen :: Int64
                    , pcmFileData :: BL.ByteString           }

data PCMFileAttributes
  = PCMFileAttributes { fileNChannels :: Int
                      , fileSampledur :: SampleDuration
                      , fileSampleT :: SampleTypeDescr  }

data SampleTypeDescr = Sample_int16le
                     | Sample_int24le
                     deriving(Eq)

data FileLoadFailure = FileIOFailure IOError
                     | FileBadWAVHeader String
                     | FileUnsuitableFormat String
                     | FileBadNSampleBits Int
                     | FileLoadOtherError String
                     deriving(Eq)
instance Show FileLoadFailure where
  show (FileIOFailure err)        = "FileIOFailure " ++ show err
  show (FileBadWAVHeader msg)     = "Bad WAV header — " ++ msg
  show (FileUnsuitableFormat fmt) = "Unsuitable WAV format \"" ++ fmt ++ "\""
  show (FileBadNSampleBits n)     = "Nonimplemented wav file bit depth " ++ show n
  show (FileLoadOtherError msg)   = "WAV file loading error " ++ msg


instance Error FileLoadFailure where
  strMsg = FileLoadOtherError


type FileLoadTrialT = ErrorT FileLoadFailure



-- | Load one channel (for a stereo file, 0 is left and 1 is right) from a simple .wav file
-- (microsoft RIFF WAVE). Currently, linear PCM in either 16 or 24 bits are the
-- only supported sample formats, but those are also by far the most widespread
-- ones and should in fact suffice for many applications.

loadWAVfileChan :: Int                                -- ^ Id of the channel to extract
                -> Timecode                           -- ^ Starting time of the file data in the desired audio
                -> FilePath                           -- ^ Path of a @.wav@-file to open.
                -> IO (Either FileLoadFailure Audio)  -- ^ Audio contents of the file
loadWAVfileChan n t₀wav = runErrorT . fmap builder . readWAVfile
 where builder :: FileContents -> Audio
       builder ( PCMFileContents (PCMFileAttributes nChannels δs spleT)
                                 dataLen audioData                      )
                   = silence `t₀Switch` sound `tEndSwitch` silence
        where sound = resampled (Resample_Affine δs) . fromPCM $ Timeline line
              line δt t₀ tpre = TimePresentation (TimeRendering $
                                                     buildFrom dataStart nSplesFromStart )
                                                 nPreload
               where buildFrom :: BL.ByteString -> Int -> Sampling -> (AudioChunk, TimeRendering Sampling AudioChunk)
                     buildFrom dataState nSplesToPlay (PCM δs')   -- δs' ≡ δs, ensured by 'resampled'

                      | nSplesToPlay>chunkLength  = (`runGet`dataState) $ do
                          chnk <- VU.replicateM chunkLength readSample
                          remain <- getRemainingLazyByteString
                          return ( AudioChunk . Just $ SampleCnt (PCM δs) chnk
                                 , TimeRendering $ buildFrom remain
                                                    (nSplesToPlay-chunkLength) )

                      | otherwise                 = (`runGet`dataState) $ do
                          chnk <- VU.replicateM nSplesToPlay readSample
                          let filled = chnk VU.++ VU.replicate
                                                   (chunkLength-nSplesToPlay) 0
                          return ( AudioChunk . Just $ SampleCnt (PCM δs) filled
                                 , silentChunks                                  )
                                   
                        
                     nSplesFromStart = fromIntegral (BL.length dataStart)
                                            `div` fromIntegral blockAlign
                     dataStart
                       | t₀Play `laterThan` t₀wav
                                    = BL.drop ( fromIntegral $ blockAlign * round
                                                  ( (t₀Play @-@ t₀wav) %/% δs )   )
                                              audioData
                       | otherwise  = BL.append ( BL.replicate
                                                    ( fromIntegral $ blockAlign * round
                                                        ( (t₀wav @-@ t₀Play) %/% δs ) )
                                                    0                                   )
                                                audioData
                                     
                     nPreload = ceiling $ tpre %/% δt
                     t₀Play = t₀ @-% nPreload *% δt
                     
                     chunkLength = round $ δt %/% δs
                     
                     readSample :: Get AudioSample
                     readSample
                       | nChannels == 1    = getSample
                       | n == 0            = do res <- getSample
                                                postSkip
                                                return res
                       | n == nChannels-1  = do preSkip
                                                getSample
                       | otherwise         = do preSkip
                                                res <- getSample
                                                postSkip
                                                return res
                     
                     preSkip  = skip $ n*chanBlockAlign                    -- go to the desired channel
                     postSkip = skip $ (nChannels - n - 1)*chanBlockAlign  -- make sure next read starts at block align
                     
                     getSample :: Get AudioSample
                     getSample = case spleT of
                        Sample_int16le -> fmap ((/2^15) . fromIntegral) getInt16le
                        Sample_int24le -> fmap ((/2^23) . fromIntegral) getInt24le

                     
              tWav = (dataLen `div` fromIntegral blockAlign) *% δs
              tEnd = t₀wav @+% tWav
              t₀Switch = switchAt t₀wav
              tEndSwitch = switchAt tEnd

              blockAlign, chanBlockAlign :: Int
              chanBlockAlign = case spleT of
                              Sample_int16le -> 2
                              Sample_int24le -> 3
              blockAlign = fromIntegral nChannels * chanBlockAlign



data ReadableAudioFmt = PCMfmt



readWAVfile :: FilePath -> FileLoadTrialT IO FileContents
readWAVfile fname = catchIOExceptions $
      liftIO (BL.readFile fname) >>= runGetTrial getWavContents




getWavContents :: FileLoadTrialT Get FileContents
getWavContents = do
      
      riffMainChunkID <- lift $ getByteString 4
      riffMainChunkID/="RIFF" ⟿⚡ FileBadWAVHeader "not an RIFF file"
      
      riffMainChunkSize <- lift getWord32le
      
      riffType <- lift $ getByteString 4
      riffType/="WAVE" ⟿⚡ FileBadWAVHeader "non-WAVE–RIFF file"
      
      fmtChunkID <- lift $ getByteString 4
      fmtChunkID/="fmt "  ⟿⚡ FileBadWAVHeader "format chunk missing or misplaced (should be the first sub-chunk)"
      
      fmtChunkSize <- lift getWord32le
      
      fmtChunk <- lift . getLazyByteString $ fromIntegral fmtChunkSize
      
      formatInfo <- mapErrorT (return . (`runGet` fmtChunk)) getWavFmtChunkCnts
      
      dataLen <- lift findDataChunk_getItsLength
      
      audioData <- lift getRemainingLazyByteString
      
      return $ PCMFileContents formatInfo
                               dataLen
                               audioData




getWavFmtChunkCnts :: FileLoadTrialT Get PCMFileAttributes
getWavFmtChunkCnts = do
      fmtAudioID <- lift getWord16le
      let fmtAudioCase = case fmtAudioID of
                            0x0001 -> Right PCMfmt
                            0x0002 -> Left "MS ADPCM"
                            0x0003 -> Left "IEEE FLOAT"
                            0x0005 -> Left "IBM CVSD"
                            0x0006 -> Left "ALAW"
                            0x0007 -> Left "MULAW"
                            0x0010 -> Left "OKI ADPCM"
                            0x0011 -> Left "DVI/IMA ADPCM"
                            0x0012 -> Left "MEDIASPACE ADPCM"
                            0x0013 -> Left "SIERRA ADPCM"
                            0x0014 -> Left "G723 ADPCM"
                            0x0015 -> Left "DIGISTD"
                            0x0016 -> Left "DIGIFIX"
                            0x0017 -> Left "DIALOGIC OKI ADPCM"
                            0x0020 -> Left "YAMAHA ADPCM"
                            0x0021 -> Left "SONARC"
                            0x0022 -> Left "DSPGROUP TRUESPEECH"
                            0x0023 -> Left "ECHOSC1"
                            0x0024 -> Left "AUDIOFILE AF36"
                            0x0025 -> Left "APTX"
                            0x0026 -> Left "AUDIOFILE AF10"
                            0x0030 -> Left "DOLBY AC2"
                            0x0031 -> Left "GSM610"
                            0x0033 -> Left "ANTEX ADPCME"
                            0x0034 -> Left "CONTROL RES VQLPC"
                            0x0035 -> Left "CONTROL RES VQLPC"
                            0x0036 -> Left "DIGIADPCM"
                            0x0037 -> Left "CONTROL RES CR10"
                            0x0038 -> Left "NMS VBXADPCM"
                            0x0039 -> Left "CS IMAADPCM (Roland RDAC)"
                            0x0040 -> Left "G721 ADPCM"
                            0x0050 -> Left "MPEG-1 Layer I, II"
                            0x0055 -> Left "MPEG-1 Layer III (MP3)"
                            0x0069 -> Left "Xbox ADPCM"
                            0x0200 -> Left "CREATIVE ADPCM"
                            0x0202 -> Left "CREATIVE FASTSPEECH8"
                            0x0203 -> Left "CREATIVE FASTSPEECH10"
                            0x0300 -> Left "FM TOWNS SND"
                            0x1000 -> Left "OLIGSM"
                            0x1001 -> Left "OLIADPCM"
                            0x1002 -> Left "OLICELP"
                            0x1003 -> Left "OLISBC"
                            0x1004 -> Left "OLIOPR"
                            _      -> Left "unknown format"
      case fmtAudioCase of
         Right _  -> return ()
         Left fmt -> throwError $ FileUnsuitableFormat
                        (fmt ++ "[ID " ++ showHex fmtAudioID "]")
      
      nChannels <- lift getWord16le
      
      sRate <- lift getWord32le
      byteRate <- lift getWord32le
      
      blockAlign <- lift getWord16le
      
      bitsPerSample <- lift getWord16le
      
      blockAlign /= (nChannels * bitsPerSample + 7) `div` 8
          ⟿⚡ FileBadWAVHeader ("blockAlign (" ++ show blockAlign
                                 ++ ") does not match ⎡nChannels*bitsPerSample/8⎤ (⎡"
                                 ++ show nChannels ++ "*" ++ show bitsPerSample ++ " / 8⎤"
                                 ++ " ≡ " ++ show ((nChannels * bitsPerSample + 7) `div` 8) ++ ")" )
      
      
      byteRate /= sRate * fromIntegral blockAlign
          ⟿⚡ FileBadWAVHeader ("byteRate (" ++ show byteRate
                                 ++ " Hz) does not match sampleRate*⎡nChannels*bitsPerSample/8⎤ ("
                                 ++ show sRate ++ " Hz * " ++ show nChannels ++ "*" ++ show bitsPerSample ++ " / 8"
                                 ++ " ≡ " ++ show (sRate * fromIntegral blockAlign) ++ " Hz)" )
      
      sampleT <- case fromIntegral bitsPerSample of
                   16 -> return Sample_int16le
                   24 -> return Sample_int24le
                   n  -> throwError $ FileBadNSampleBits n
      
      return $ PCMFileAttributes (fromIntegral nChannels)
                                 (oneSecond %/ sRate)
                                 sampleT



   

findDataChunk_getItsLength :: Get Int64
findDataChunk_getItsLength = do
   chunkID <- getByteString 4
   chunkLength <- getWord32le
   if chunkID == "data"
        then return $ fromIntegral chunkLength
        else do
           skip $ fromIntegral chunkLength
           findDataChunk_getItsLength



runGetTrial :: Monad m => FileLoadTrialT Get a -> BL.ByteString -> FileLoadTrialT m a
runGetTrial getter bytes = mapErrorT (return . (`runGet` bytes)) getter


catchIOExceptions :: FileLoadTrialT IO FileContents -> FileLoadTrialT IO FileContents
catchIOExceptions = mapErrorT (`catch`(return . Left . FileIOFailure))


infix 1 ⟿⚡
(⟿⚡) :: Monad m => Bool -> FileLoadFailure -> FileLoadTrialT m ()
cond ⟿⚡ errC = when cond $ throwError errC


getInt16le :: Get Int16
getInt16le = fmap fromIntegral getWord16le

getInt24le :: Get Int32
getInt24le = do
   lsb <- getWord8
   msb <- getInt16le
   return $ (fromIntegral msb * 256) .|. fromIntegral lsb


getInt32le :: Get Int32
getInt32le = fmap fromIntegral getWord32le

getInt64le :: Get Int64
getInt64le = fmap fromIntegral getWord64le

