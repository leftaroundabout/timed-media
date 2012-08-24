module MMedia.Timecode.Internal where


newtype Timecode = Timecode { getTimecode :: Double }
newtype RelTime  = RelTime  { seconds     :: Double }

newtype Frequency = Frequency { hertzs :: Double }




