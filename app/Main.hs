module Main (main) where

import qualified Data.Map.Strict as M
import Data.Char (ord, toLower, isAsciiLower, isAsciiUpper, isSpace)
import Data.List.Split (splitOn)
import Data.List (foldl')
import Text.Read (readMaybe)
import Data.Array ( Array, (!), array )
import qualified Data.Map.Lazy   as ML


--hard-coded file paths
wikiFile :: FilePath
wikiFile = "enwiki-2023-04-13.txt"

entropyFile :: FilePath
entropyFile = "entropy.txt"

msgFile :: FilePath
msgFile = "ishmael.dec"

segOutFile :: FilePath
segOutFile = "ishmael_seg.dec"

minCount :: Int
minCount = 50

--prepare function produces the entropy document.
prepare :: IO ()
prepare = do
  contents <- readFile wikiFile
  let mp0 = foldl' stepLine M.empty (lines contents)
      mp  = M.filter (>= minCount) mp0
      total :: Double
      total = fromIntegral (sum (M.elems mp))
      outLines =
        [ w ++ " " ++ show (entropy total c) | (w, c) <- M.toAscList mp ]
  writeFile entropyFile (unlines outLines)

-- Accumulate counts from one input line into the map
stepLine :: M.Map String Int -> String -> M.Map String Int
stepLine acc line =
  case parseLine line of
    Nothing -> acc
    Just (rawWord, num)
      | not (all isAsciiChar rawWord) -> acc
      | otherwise ->
          let parts = splitOnHyphen rawWord
              subws = filter (not . null) (map normalize parts)
          in  foldl' (\counts word -> M.insertWith (+) word num counts) acc subws

-- Parse word count when given a map entry
parseLine :: String -> Maybe (String, Int)
parseLine line =
  case words line of
    [word, nstr] -> do
      num <- readMaybe nstr
      pure (word, num)
    _ -> Nothing

normalize :: String -> String
normalize = map toLower . filter isAsciiLetter

isAsciiLetter :: Char -> Bool
isAsciiLetter c = isAsciiLower c || isAsciiUpper c

isAsciiChar :: Char -> Bool
isAsciiChar c = ord c < 128

splitOnHyphen :: String -> [String]
splitOnHyphen = splitOn "-"

entropy :: Double -> Int -> Double
entropy total c =
  let p = fromIntegral c / total
  in  negate (logBase 2 p)


--Part II segmentation main function
segment :: IO ()
segment = do
  dict <- loadEntropyMap entropyFile
  msgRaw <- readFile msgFile
  let msg = filter (not . isSpace) msgRaw
      (entropyArr, wordArr) = buildcostarray dict msg
      wordsOut = reconstruct msg wordArr
      out = wrapLine (unwords wordsOut)
  writeFile segOutFile out
  putStr out

-- Load entropy.txt
loadEntropyMap :: FilePath -> IO (ML.Map String Double)
loadEntropyMap fp = do
  contents <- readFile fp
  let pairs = foldr step [] (lines contents)
  pure (ML.fromAscList pairs)
  where
    step line acc =
      case words line of
        [word, entropy_str] ->
          case readMaybe entropy_str of
            Just entropy  -> (word, entropy) : acc
            Nothing -> acc
        _ -> acc


buildcostarray :: ML.Map String Double -> String -> (Array Int Double, Array Int String)
buildcostarray dict msg =
  let n = length msg
      inf = 1/0 :: Double
      --helper function to decide what word choice produces the minimum total entropy
      bestAt :: Array Int Double -> Int -> (Double, String)
      bestAt entropyArr i =
        let sfx = drop i msg
            cands = prefixes dict sfx
            scored =
              [ (entropy + entropyArr ! (i + length w), w) | (w, entropy) <- cands, i + length w <= n ]
        in case scored of
             [] -> (inf, "")
             _  -> minimum scored

      entropyArr :: Array Int Double
      entropyArr =
        array (0, n)
          ( [ (i, costAt i) | i <- [0 .. n-1] ] ++ [(n, 0.0)] )
        where
          costAt i = fst (bestAt entropyArr i)

      wordArr :: Array Int String
      wordArr =
        array (0, n-1)
          [ (i, snd (bestAt entropyArr i)) | i <- [0 .. n-1] ]

  in (entropyArr, wordArr)

maxWordLen :: Int
maxWordLen = 30

--filter prefixes to only dictionary words with entropy.
prefixes :: ML.Map String Double -> String -> [(String, Double)]
prefixes dict sfx =
  let ps = take maxWordLen (prefixesOf sfx)
  in foldl' (\acc p -> case ML.lookup p dict of
                         Just e  -> (p,e):acc
                         Nothing -> acc)
            []
            ps

--produce a list of strings which are direct prefixes of the following message, but does not necessarily are dictionary words.
--helper for prefixes.
prefixesOf :: String -> [String]
prefixesOf = go ""
  where
    go _ [] = []
    go pre (c:cs) =
      let pre' = pre ++ [c]
      in pre' : go pre' cs

-- Reconstruct the best segmentation
reconstruct :: String -> Array Int String -> [String]
reconstruct msg wordArr = go 0
  where
    n = length msg
    go i
      | i >= n = []
      | otherwise =
          let w = wordArr ! i
          in if null w
               then [drop i msg]
               else w : go (i + length w)

wrapLine :: String -> String
wrapLine = unlines . go
  where
    go [] = []
    go s  = let (a,b) = splitAt 60 s
            in a : go b

main :: IO ()
main = do
  prepare
  segment