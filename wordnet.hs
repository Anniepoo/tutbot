module WordNet ( load_word_net )  where
import System.IO
import qualified Data.HashTable.IO as H

-- Using concrete type makes it easier for ghc
-- to optimize
type HashTable k v = H.BasicHashTable k v

-- s(synset_id,w_num,’word’,ss_type,sense_number,tag_count).
data SRecord = SRecord {
               wordNum :: Int,
               word :: String,
               synsetType :: Char,
               sense :: Int,
               tagged :: Int -- # times sense in concordance
               }

newtype WordNetSTable  = WordNetSTable {
                      shash :: (HashTable Int SRecord)
                      }

-- newht1 :: HashTable Int SRecord
newht1 = do
       ht <- H.new
       return ht

ht :: HashTable Int SRecord
ht = newht1
wordnetS = WordNetSTable ht

-- hyp(synset_id,synset_id).
-- The hyp operator specifies that the second synset is a
-- hypernym of the first synset.
newtype WordNetHypernymTable = WordNetHypernymTable {
                             hhash :: HashTable Int Int
                             }

wordnetH :: WordNetHypernymTable
wordnetH  = WordNetHypernymTable (HashTable Int Int)

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
