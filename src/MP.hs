module MP (separators, notpuncnospacesep, lookUp, splitText, combine, getKeywordDefs, expand, expandonce, replaceWord) where

import Data.Maybe
import Data.List

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators = " \n\t.,:;!\"\'()<>/\\"
notpuncnospacesep :: String
notpuncnospacesep = "\n\t()<>/\\" --punctuation marks and space are both separator chars but I can't see why in the sentences in test data there are space are punctuation marks acting as a part of the sentence

-----------------------------------------------------

{-|
This function will look up a key in a list of key-value pairs,
returning all the values that match with that key.

> lookUp "A" [("A", 8), ("B", 9), ("C", 5), ("A", 7)] == [8, 7]
-}
lookUp :: String -> [(String, a)] -> [a]
lookUp key pairs = [snd x | x<-pairs, fst x == key]

{-|
This function will break up a string with some given separator
characters, returning both the list of separators found between
each "word" and the words themselves.
-}
splitText :: [Char] -- ^ the separators to split on
          -> String -- ^ the string to split
          -> ([Char], [String])
splitText _ "" = ([], [""])
splitText sep (c : str)
  | c `elem` sep = let (sepc, sepwords) = splitText sep str
                   in (c : sepc, "" : sepwords)
  | otherwise = let (spec, sepwords) = splitText sep str
                in (spec, (c : head sepwords) : tail sepwords)

{-|
This function interleaves the characters from the first argument
list with the strings in the second argument. The second list must
be non-empty.
-}
combine :: [Char] -> [String] -> [String]
combine _ [] = [""]
combine _ [s] = [s]
combine [] x = x
combine (c : sep) (s1 : strs) = s1 : [c] : combine sep strs

{-|
This function takes a list of lines and splits each line to
extract a list of keyword-definition pairs.

> getKeywordDefs ["$x Define x", "$y 55"] == [("$x", "Define x"), ("$y", "55")]
-}
getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs [] = []
getKeywordDefs (str : strs) = let sptext = splitText [' '] str
                                  keyword = head (snd sptext)
                                  value = concat (combine (tail (fst sptext)) (tail (snd sptext)))
                                  in (keyword, value) : getKeywordDefs strs

{-|
This function takes the contents of two files, one containing
a template and the other the definitions for each keyword
found within the template. It extracts the keyword-definition
information from the info file and uses it to expand the occurrences
of these keywords in the template file, producing new file contents
as a result.

> expand "The capital $1 is $2" "$1 Peru\n$2 Lima." == "The capital of Peru is Lima"
                                                                                  /\ no '.' here, guess I'm right about the test data
                                                                                  /\ OK the test data wins I will adjust my code to fit the test data
-}
expand :: FileContents -- ^ the template file contents
       -> FileContents -- ^ the info file contents
       -> FileContents
expand template defs
  |  '#' `notElem` defs = expandonce template defs
  |  otherwise = let keys = snd (splitText "#" defs)
                     texts = map (expandonce template) keys
                     in intercalate "-----\n" texts ++ "-----\n"

--expandonce was the original expand but had to write a new one for the extension content
expandonce :: FileContents -- ^ the template file contents
       -> FileContents -- ^ the info file contents
       -> FileContents
expandonce template defs = concat (combine (fst words) (map (`replaceWord` keys) (snd words))) -- replace all words in the text by keys and then combine them all together to get a string
  where words = splitText separators template
        keys = getKeywordDefs (snd (splitText notpuncnospacesep defs)) --punctuation marks and space should not be in the separators I think

-- You may wish to uncomment and implement this helper function
-- when implementing expand
replaceWord :: String -> KeywordDefs -> String
replaceWord "" _ = ""
replaceWord word keys
  | head word == '$' = fromMaybe word (listToMaybe (lookUp word keys)) -- not sure this is how it should be done. I found out about the maybe thing on the Internet and it did work here so I wrote something like this 
  | otherwise = word
