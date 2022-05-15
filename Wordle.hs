import Data.List
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Words

green :: String -> String -> [Bool]
green [] [] = []
green (x:xs) (y:ys) = (x == y) : (green xs ys)

yellow :: String -> String -> [Bool]
yellow hidden guess =
  let greens = green hidden guess
      non_greens = S.fromList $ snd <$> filter fst (zip (not <$> greens) hidden)
      y :: [Bool] -> S.Set Char -> String -> [Bool]
      y _ _ []       = []
      y (g:gs) ngs (x:xs) = if g
        then False : (y gs ngs xs)
        else if x `S.member` ngs
          then True : (y gs (S.delete x ngs) xs)
          else False : (y gs ngs xs)
  in y greens non_greens guess

data Match = G | Y | X deriving (Show, Eq, Ord)
type Result = [Match]

toResult :: String -> Result
toResult = map f
  where
    f 'g' = G
    f 'y' = Y
    f 'x' = X

query :: String -> String -> Result
query hidden guess =
  f <$> (zip (green hidden guess) (yellow hidden guess))
  where
    f (True, False) = G
    f (False, True) = Y
    f (False, False) = X

possible :: [(String, Result)] -> String -> Bool
possible results hidden = and (map (f hidden) results)
  where
    f :: String -> (String, Result) -> Bool
    f hidden (guess, result) = query hidden guess == result

validWords :: [(String, Result)] -> [String]
validWords results = filter (possible results) finalWords

partitionWords :: [String] -> String -> [Int]
partitionWords possible_words guess =
  map length $ M.elems $ foldl' go M.empty possible_words
    where
      go :: M.Map Result [String] -> String -> M.Map Result [String]
      go parts hidden =
        let answer = query hidden guess
        in M.alter al answer parts
          where
            al :: Maybe [String] -> Maybe [String]
            al Nothing = Just [hidden]
            al (Just xs) = Just (hidden : xs)

expectedNumberOfEliminatedWords :: Int -> [Int] -> Double
expectedNumberOfEliminatedWords total lengths =
  sum [ len / t * (t - len) | len <- map fromIntegral lengths ]
    where
      t = fromIntegral total

expectedValueOfGuess :: [String] -> String -> Double
expectedValueOfGuess possible_words guess =
  expectedNumberOfEliminatedWords
    (length possible_words) (partitionWords possible_words guess)

playWordle :: [(String, Result)] -> IO ()
playWordle results = do
  let possible_words = validWords results
  let vs = map (expectedValueOfGuess possible_words) guessWords 
  let next_guess = if (length possible_words) > 2
                     then snd $ maximum $ zip vs guessWords
                     else head possible_words
  putStrLn next_guess
  response <- getLine
  if response /= "ggggg"
    then playWordle $ (next_guess, toResult response) : results
    else putStrLn "Thank you!"

main :: IO ()
main = do
  playWordle []
  main
