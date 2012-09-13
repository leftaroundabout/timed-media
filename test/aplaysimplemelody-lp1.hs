import Media.Timed.Timecode.Arith
import Media.Timed.Audio.Example
import Media.Timed.Audio.ALSAPlay
import Media.Timed.Audio.FX.Filter.SimpleIIR

main = aplaySimple (timecode $ 0:0:[0]) $ mix [ myLp.myLp $ melodyLoop
                                              , myLp.myLp.myLp.myLp.myLp.myLp $ bassLoop ]
  where melodyLoop = timeChain [ (timecode [s], melody) | s<-take 50000  [0,    8 ..] ]
        bassLoop   = timeChain [ (timecode [s], bass  ) | s<-take 100000 [0, 4    ..] ]
        
        melody = timeChain $ zip [ timecode [s] | s<-[0, 1/4 ..] ] melotones
        bass   = timeChain $ zip [ timecode [s] | s<-[0 ..] ] basstones
        
        melotones = [ tone $ (330 * 2**(n/12)) *~ oneHertz
                     | n<-[0, 2, 3, 5, 7, 8, 7, 7, 5, 3, 5, 7, 11, 7, 12, 15, 12] ]
        basstones = [ tone $ (165 * 2**(n/12)) *~ oneHertz
                     | n<-[0,          3,          8,          7,             0 ] ]
        
        tone = simpleDecayTone 2 (1/11) oneHertz
        
        myLp = lpOrder1 (1500*~oneHertz)