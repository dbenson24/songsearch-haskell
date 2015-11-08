module Word(Word (..)) where
    data Word = Word {
                        word :: String,
                        songid :: Int,
                        positions :: [Int]
                    } deriving (Show)