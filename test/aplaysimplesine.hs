import MMedia.Audio.Example
import MMedia.Audio.ALSAPlay

main = do
   putStrLn "quun"
   aplaySimple (timecode $ 0:0:[0]) $ sineDecaying oneHertz (440 *~ oneHertz)