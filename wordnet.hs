module WordNet ( load_word_net )  where
import System.IO


load_word_net :: FilePath -> IO ()
load_word_net filename =
      withFile filename ReadMode readData

-- load wordnet into a hash
readData :: Handle -> IO ()
readData h = do
         isEnd <- hIsEOF h
         if isEnd then
            return ()
         else
            do
                s <- hGetLine h
                putStrLn s
                readData h
