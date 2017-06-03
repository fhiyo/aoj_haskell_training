import           Control.Arrow
import           Data.List
import           Data.Ord

main :: IO ()
main =
  getContents >>=
  putStrLn .
  (\(f, s) -> f ++ " " ++ s) .
  extractFreqAndLongestWord .
  map (length &&& head) . makeFreqList [[]] . sort . words

extractFreqAndLongestWord :: [(Int, String)] -> (String, String)
extractFreqAndLongestWord list =
  ( snd (minimumBy (flip compare) list)
  , snd . head $ sortBy (flip . comparing $ length . snd) list)

makeFreqList :: [[String]] -> [String] -> [[String]]
makeFreqList nested_list [] = nested_list
makeFreqList [[]] v_list@(v:vs) = makeFreqList [[v]] vs
makeFreqList [] v_list@(v:vs) = makeFreqList [[v]] vs
makeFreqList nested_list@(x:xs) v_list@(v:vs) =
  if head x == v
    then makeFreqList ((x ++ [v]) : xs) vs
    else x : makeFreqList xs v_list
