import MMedia.Timecode.Arith
import MMedia.Audio.SimpleFileIO
import MMedia.Audio.ALSAPlay

import System.Environment

import Control.Monad

main = do
   args <- getArgs
   
   when (length args /= 1) $
      error "Need exactly one argument: the file name of a valid .wav file."
   
   wavContents <- loadWAVfileChan 0 (timecode(0:0:[0])) $ head args

   case wavContents of
      Left problem -> print problem
      Right audio -> aplaySimple -- Logged "testsound0-aplayed0.rawcdaa"
                          (timecode(0:0:[0])) audio