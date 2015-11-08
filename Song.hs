module Song(Song (..)) where 
    data Song = Song { 
                        title :: String,
                        artist :: String,
                        words :: [String],
                        id :: Int
                    } deriving (Show)
