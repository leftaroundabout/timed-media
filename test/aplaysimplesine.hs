import MMedia.Audio.Example
import MMedia.Audio.ALSAPlay

main = do
   aplaySimple -- Logged "testsine0-aplayed.rawcdaa" (timecode $ 0:0:[0]) $ sineDecaying oneHertz (440 *~ oneHertz)