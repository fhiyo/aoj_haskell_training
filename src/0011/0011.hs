import           Data.List.Split

main :: IO ()
main =
  getContents >>=
  mapM_ print .
  swapList .
  (\list ->
     let natural_list = ((\num -> [1 .. num]) . head . head) list
         swapping_list = map (map (\x -> x - 1)) $ (tail . tail) list
     in (natural_list, swapping_list)) .
  map (map (read :: String -> Int) . splitOn ",") . lines

swapList :: ([Int], [[Int]]) -> [Int]
swapList (natural_list, swapping_list) =
  if null swapping_list
    then natural_list
    else let natural_list' = swap (head swapping_list) natural_list
             swapping_list' = tail swapping_list
         in swapList (natural_list', swapping_list')

swap :: [Int] -> [a] -> [a]
swap [i, j] xs = swap' $ zip [0 ..] xs
  where
    swap' = map f
      where
        f (index, x)
          | index == i = xs !! j
          | index == j = xs !! i
          | otherwise = x
