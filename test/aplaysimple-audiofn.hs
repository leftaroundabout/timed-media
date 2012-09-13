import Media.Timed.Timecode.Arith
import Media.Timed.Audio.ALSAPlay

main = aplaySimple (timecode $ 0:0:[0]) $ audioFn NoAntiAliasing f
  where f t = wfn ω / 2 + wfn(5/4 *ω) / 3
          where ω = 2*pi * (ν ~*% (t @-@ timeZero))
        ν = 440 *~ oneHertz
        wfn ω = dst $ sin ω
          where dst a = a / (1 + a^2)