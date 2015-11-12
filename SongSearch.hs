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
    let songs = map Song.parseSong (zip [0..] (Split.splitOn "<BREAK>\n" (C.unpack s)))
    let lyrics = getLyricMap songs
    return (songs, lyrics)
    
main = do
    db <- run
    contents <- getContents
    mapM_ (answerQuery db) $ lines contents

answerQuery :: ([Song.Song], Map String [Word.Word]) -> String -> IO()
answerQuery db line = do 
                        putStr (searchLyric db line)
                        

searchLyric :: ([Song.Song], Map String [Word.Word]) -> String -> String
searchLyric (songs, lyrics) input = case val of Nothing -> "The query was not found!\n"
                                                Just val -> Song.normalizeLyricResults songs val 
                                where   query = [x | x <- input, x `elem` ['a'..'z'] || x `elem` ['A'..'Z']]
                                        val = Map.lookup query lyrics

getLyricMap :: [Song.Song] -> Map String [Word.Word]
getLyricMap xs = generateLyricMap (concat (map Song.reduceToLyrics xs))

foldLyrics :: Map String [Word.Word] -> (String, Word.Word) -> Map String [Word.Word]
foldLyrics m (key, word) = case val of Nothing -> Map.insert key [word] m
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

generateLyricMap :: [(String, Word.Word)] -> Map String [Word.Word]
generateLyricMap xs = foldl foldLyrics Map.empty xs

testWord = Word.Word{Word.word="Test", Word.songid=1, Word.positions=[1,2,4,5]}
testWord' = Word.Word{Word.word="Test", Word.songid=1, Word.positions=[6,7,8]}
testWord'' = Word.Word{Word.word="Hello", Word.songid=1, Word.positions=[6,7,8]}
testTuple = (Word.word testWord, testWord)
testTuple' = (Word.word testWord', testWord')
testTuple'' = (Word.word testWord'', testWord'')

tupleList = [testTuple, testTuple', testTuple'']