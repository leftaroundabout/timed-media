module MMedia.Audio.ALSAPlay where

import MMedia.Audio
import System.Process
import Data.Binary.Put

import Data.Bytestring.Lazy

aplaySimple :: Timecode -> Audio -> IO ()
aplaySimple t₀ aud = do
    (aplaySTDIn, _, _, aplayProc) <- runInteractiveCommand "aplay -f cd"
    forM_ (runTimeline (1/10 *% oneSecond) t₀) (\chunk ->
       let putter = forM_ (renderAudioChunk (1/44100 *% oneSecond)) (\sample -> do
          let outSple = round . max (-32768) . min 32767 $ sample * 32768 :: Int16
          put outSple
          put outSple
       in hPut aplaySTDIn $ encode putter
     )
