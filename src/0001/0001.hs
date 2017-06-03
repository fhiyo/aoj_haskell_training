import           Data.List

-- main :: IO ()
-- main = getContents >>= mapM_ print . take 3 . reverse . sort . map (read :: String -> Int) . words
-- (reverse . sort)よりも(flip compare)の方が高速？ https://ro-che.info/articles/2016-04-02-descending-sort-haskell
main :: IO ()
main =
  getContents >>=
  mapM_ print .
  take 3 . sortBy (flip compare) . map (read :: String -> Int) . words
