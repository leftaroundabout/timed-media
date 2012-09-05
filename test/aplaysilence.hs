import MMedia.Timecode.Arith
import MMedia.Audio.ALSAPlay

main = aplaySimple (timecode $ 0:0:[0]) silence
