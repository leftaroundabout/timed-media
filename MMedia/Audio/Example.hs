module MMedia.Audio.Example where

import MMedia.Audio

sineTest :: Audio
sineTest = Timeline $
              \δt t₀ -> 
                [ AudioChunk δt (\ δs -> [ realToFrac $ sin t
                                          | t <- iterate(@+ δs) tᵢ ] )
                 | tᵢ <- iterate(@+ δt) timeZero ]
