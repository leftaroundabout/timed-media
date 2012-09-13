import Media.Timed.Timecode.Arith
import Media.Timed.Audio.ALSAPlay

main = aplaySimple (timecode $ 0:0:[0]) silence
