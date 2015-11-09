module Song(Song (..), parseSong, reduceToLyrics) where 
    import qualified Data.List.Split as Split
    import qualified Word
    import Data.List
    data Song = Song { 
                        title :: String,
                        artist :: String,
                        words :: [String],
                        id :: Int
                    } deriving (Show)
                    
    parseSong :: (Int, String) -> Song.Song
    parseSong (sid, "") = Song.Song{Song.title="", Song.id=(-1), Song.artist="", Song.words=[""]}
    parseSong (sid, text) = Song.Song title artist lyrics sid
            where (artist:title:rest) = Split.splitOn "\n" text
                  lyrics = Split.splitOn " " (intercalate " " rest)
                  
    parseLyrics :: [String] -> Int -> [(String, Word.Word)]
    parseLyrics [] sid = []
    parseLyrics xs sid = [(word, Word.Word{Word.word=word, Word.songid=sid, Word.positions=[i]}) | (i, word) <- zip [0..] xs]

    reduceToLyrics :: Song.Song -> [(String, Word.Word)]
    reduceToLyrics song = parseLyrics (Song.words song) (Song.id song)