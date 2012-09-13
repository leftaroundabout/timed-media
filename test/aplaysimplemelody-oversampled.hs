import Media.Timed.Timecode.Arith
import Media.Timed.Audio.Example
import Media.Timed.Audio.ALSAPlay

main = aplaySimple (timecode $ 0:0:[0]) oversamplings
  where oversamplings = timeChain $  [ ( timecode [fromIntegral (s * 8) * nOctaves]
                                      , resampled(Oversample_Averaging $ s+1) audio)
                                     | s <- [0 .. maxOversample - 1] ]
                                 ++  [ ( timecode [fromIntegral ((s + maxOversample)*8) * nOctaves]
                                      , resampled(Resample_Affine
                                                    $ oneSecond %/ (48000 + 10000*s) ) audio)
                                     | s <- [0 .. maxOversample - 1] ]
        
        audio = gain (1/6) $ mix [melodyLoop, bassLoop]
        
        melodyLoop = timeChain [ (timecode [s], melody oct) | (s,oct) <- zip
                                                            (take 50000  [0,    8 ..]) (cycle [0 .. nOctaves-1]) ]
        bassLoop   = timeChain [ (timecode [s], bass  ) | s<-take 100000 [0, 4    ..] ]
        
        melody = timeChain . zip [ timecode [s] | s<-[0, 1/4 ..] ] . melotones 
        bass   = timeChain $ zip [ timecode [s] | s<-[0 ..] ] basstones
        
        melotones oct = [ tone $ (330 * 2**(n/12 + oct)) *~ oneHertz
                     | n<-[0, 2, 3, 5, 7, 8, 7, 7, 5, 3, 5, 7, 11, 7, 12, 15, 12] ]
        basstones = [ tone $ (165 * 2**(n/12)) *~ oneHertz
                     | n<-[0,          3,          8,          7,             0 ] ]
        
        tone = simpleDecayTone 2 (1/11) oneHertz
        
        maxOversample = 2
        nOctaves = 4