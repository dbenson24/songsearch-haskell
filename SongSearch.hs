import qualified Song
import qualified Word
import qualified Data.ByteString.Lazy as L
import qualified Data.List.Split as Split
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map


run = do
    s <- L.readFile "rick_db.txt"
    let songs = map parseSong (Split.splitOn "<BREAK>\n" (C.unpack s))
    return songs

parseSong :: String -> Song.Song
parseSong "" = Song.Song{Song.title="", Song.id=(-1), Song.artist="", Song.words=[""]}
parseSong text = Song.Song{Song.title=title, Song.id=0, Song.artist=artist, Song.words=lyrics} 
        where (artist:title:rest) = Split.splitOn "\n" text
              lyrics = Split.splitOn " " (intercalate " " rest)
              
              
parseLyrics :: [String] -> Int -> [(String, Word.Word)]
parseLyrics [] sid = []
parseLyrics xs sid = [(word, Word.Word{Word.word=word, Word.songid=sid, Word.positions=[i]}) | (i, word) <- zip [0..] xs]

generate :: Map String [Word.Word] -> (String, Word.Word) -> Map String [Word.Word]
generate m (key, word) = case val of Nothing -> Map.insert key [word] m
                                     val -> m
                        where val = Map.lookup key m
                        
                        
testWord = Word.Word{Word.word="Test", Word.songid=1, Word.positions=[1,2,4,5]}
testKey = Word.word testWord
testTuple = (testKey, testWord)