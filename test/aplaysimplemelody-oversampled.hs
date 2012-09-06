import MMedia.Timecode.Arith
import MMedia.Audio.Example
import MMedia.Audio.ALSAPlay

main = aplaySimple (timecode $ 0:0:[0]) oversamplings
  where oversamplings = timeChain [ ( timecode [fromIntegral $ s*32]
                                    , oversampled(Oversample_Averaging $ s+1) audio)
                                    | s <- [0, 1, 2, 3, 4] ]
        
        audio = gain (1/6) $ mix [melodyLoop, bassLoop]
        
        melodyLoop = timeChain [ (timecode [s], melody oct) | (s,oct) <- zip
                                                            (take 50000  [0,    8 ..]) (cycle [0,1,2,3]) ]
        bassLoop   = timeChain [ (timecode [s], bass  ) | s<-take 100000 [0, 4    ..] ]
        
        melody = timeChain . zip [ timecode [s] | s<-[0, 1/4 ..] ] . melotones 
        bass   = timeChain $ zip [ timecode [s] | s<-[0 ..] ] basstones
        
        melotones oct = [ tone $ (330 * 2**(n/12 + oct)) *~ oneHertz
                     | n<-[0, 2, 3, 5, 7, 8, 7, 7, 5, 3, 5, 7, 11, 7, 12, 15, 12] ]
        basstones = [ tone $ (165 * 2**(n/12)) *~ oneHertz
                     | n<-[0,          3,          8,          7,             0 ] ]
        
        tone = simpleDecayTone 2 (1/11) oneHertz