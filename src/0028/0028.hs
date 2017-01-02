import Data.List
import Control.Arrow

main :: IO ()
main = getContents >>= mapM_ print . sort . map snd . (\list@(x:xs) -> filter ((== fst x) . fst) list) . sortBy (flip compare) . map (length &&& head) . makeFreqList [[]] . sort . map (read :: String -> Int) . lines

makeFreqList :: [[Int]] -> [Int] -> [[Int]]
makeFreqList nested_list [] = nested_list
makeFreqList [[]] v_list@(v:vs) = makeFreqList [[v]] vs
makeFreqList [] v_list@(v:vs) = makeFreqList [[v]] vs
makeFreqList nested_list@(x:xs) v_list@(v:vs) =
  if head x == v
  then makeFreqList ((x ++ [v]) : xs) vs
  else x : makeFreqList xs v_list


