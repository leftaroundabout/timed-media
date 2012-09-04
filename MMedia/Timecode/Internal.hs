module MMedia.Timecode.Internal where


newtype Timecode = Timecode { getTimecode :: Double } deriving (Eq)
newtype RelTime  = RelTime  { seconds     :: Double } deriving (Eq, Ord)

newtype Frequency = Frequency { hertzs :: Double }




