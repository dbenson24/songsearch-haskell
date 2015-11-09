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
                                     Just val -> Map.insert key (handleSameSong val word) m
                        where val = Map.lookup key m

-- Handes collisions on a word node, appends positions if the same node is already present, adds a new node if the same node is not present.
handleSameSong :: [Word.Word] -> Word.Word -> [Word.Word]
handleSameSong node word =  if val == sid
                                then (Word.Word{Word.word=(Word.word x), Word.songid=sid, Word.positions=(Word.positions word ++ Word.positions x)}:xs)
                                else (word:node)
                            where (x:xs) = node
                                  val = Word.songid x
                                  sid = Word.songid word
                        
testWord = Word.Word{Word.word="Test", Word.songid=1, Word.positions=[1,2,4,5]}
testWord' = Word.Word{Word.word="Test", Word.songid=1, Word.positions=[6,7,8]}
testKey = Word.word testWord
testTuple = (testKey, testWord)