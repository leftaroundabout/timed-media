import Media.Timed.Timecode.Arith
import Media.Timed.Audio.Example
import Media.Timed.Audio.ALSAPlay
import Media.Timed.Audio.FX.Filter.SimpleIIR

main = aplaySimple (timecode $ 0:0:[0]) . gain(1/4) . finalLP (6000*~oneHertz)
           $ mix [ upcLp (3000*~oneHertz) $ melodyLoop
                 , gain 2.5 . resoLp (2000*~oneHertz) $ bassLoop ]
  where melodyLoop = timeChain [ (timecode [s/bps], melody) | s<-take 50000  [0,    8 ..] ]
        bassLoop   = timeChain [ (timecode [s/bps], bass  ) | s<-take 100000 [0, 4    ..] ]
        
        melody = timeChain $ zip [ timecode [s/bps] | s<-[0, 1/4 ..] ] melotones
        bass   = timeChain $ zip [ timecode [s/bps] | s<-[0 ..] ] basstones
        
        melotones = [ lpOrder2Nonlinear intens tanh (sqrt intens *~ (1000 *~oneHertz))
                          . meltone $ (330 * 2**(n/12)) *~ oneHertz
                     | (n,intens)<-zip
                          [0, 2, 3, 5,  7, 8, 7, 7,  5, 3, 5, 7, 11, 7, 12, 15, 12]
                          [4, 2, 2, 1.5,2, 2, 2, 1.4,3, 1, 2, 3, 4,  2, 1.5,2,  3 ] ]
        basstones = [ basstone $ (165/2 * 2**(n/12)) *~ oneHertz
                     | n<-[0,          3,          8,          7,             0 ] ]
        
        meltone ν = mix [ simpleDecayTone 2   1     (16*~oneHertz) (ν~*2)
                        , simpleDecayTone 1.2 (1/2) (2*~oneHertz)    ν    ]
        basstone = simpleDecayTone 2 (1/3) (1*~oneHertz)
        
        resoLp = lpOrder2Nonlinear 24 (\x-> x + x^2/2 - x^3/3)
        upcLp = lpOrder2Nonlinear 24 (sin . subtract 1 . exp)
        finalLP = lpOrder2Saturating 2
        
        bps = 2.6