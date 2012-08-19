module MMedia.Audio where

import MMedia.Timeline

type AudioSample = Float
type SampleCnt = [AudioSample]

data AudioChunk = AudioChunk { aChunkLength :: RelTime
                             , renderAudioChunk :: RelTime    -- sample duration / inverse sample rate
                                                -> SampleCnt
                             }


type Audio = Timeline AudioChunk
                             
   