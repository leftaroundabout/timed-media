import MMedia.Timecode.Arith
import MMedia.Audio.Example
import MMedia.Audio.ALSAPlay
import MMedia.Audio.FX.Filter.SimpleIIR

main = aplaySimple (timecode $ 0:0:[0]) . gain(1/4) . finalLP
           $ mix [ upcLp $ melodyLoop
                 , gain 2.5 . resoLp $ bassLoop ]
  where melodyLoop = timeChain [ (timecode [s], melody) | s<-take 50000  [0,    8 ..] ]
        bassLoop   = timeChain [ (timecode [s], bass  ) | s<-take 100000 [0, 4    ..] ]
        
        melody = timeChain $ zip [ timecode [s] | s<-[0, 1/4 ..] ] melotones
        bass   = timeChain $ zip [ timecode [s] | s<-[0 ..] ] basstones
        
        melotones = [ tone $ (330 * 2**(n/12)) *~ oneHertz
                     | n<-[0, 2, 3, 5, 7, 8, 7, 7, 5, 3, 5, 7, 11, 7, 12, 15, 12] ]
        basstones = [ tone $ (165/2 * 2**(n/12)) *~ oneHertz
                     | n<-[0,          3,          8,          7,             0 ] ]
        
        tone = simpleDecayTone 2 (1/2) oneHertz
        
        resoLp = lpOrder2Nonlinear 24 (\x-> x + x^2/2 - x^3/3) (2000*~oneHertz)
        upcLp = lpOrder2Nonlinear 24 (sin . subtract 1 . exp) (3000*~oneHertz)
        finalLP = lpOrder2Saturating 2 (6000*~oneHertz)