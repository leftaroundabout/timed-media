module MMedia.Timecode.Internal where


newtype Timecode = Timecode { getTimecode :: Double }
newtype RelTime  = RelTime  { seconds     :: Double }

newtype Frequency = Frequency { hertzs :: Double }




oneSecond :: RelTime
oneSecond = RelTime 1

timeZero :: Timecode
timeZero = Timecode 0



oneHertz :: Frequency
oneHertz = Frequency 1
