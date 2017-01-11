import Data.List.Split

main :: IO ()
main = getContents >>= mapM_ print . fListMap [countRec, countLozenges] . map (map (read :: String -> Int) . splitOn ",") . lines

countRec :: [[Int]] -> Int
countRec = foldl (\acc [a,b,c] -> if a ^ 2 + b ^ 2 == c ^ 2 then acc + 1 else acc) 0

countLozenges :: [[Int]] -> Int
countLozenges = foldl (\acc [a,b,c] -> if a == b then acc + 1 else acc) 0

fListMap :: [a -> b] -> a -> [b]
fListMap [] value = []
fListMap flist@(x:xs) value = x value : fListMap xs value


-- main = getContents >>= mapM_ print . (\list -> [sum list, length list - sum list]) . map (\[a,b,c] -> if a ^ 2 + b ^ 2 == c ^ 2 then 1 else 0) . uniq . map (sort . map (read :: String -> Int) . splitOn ",") . lines

-- uniq :: (Eq a) => [a] -> [a]
-- uniq [] = []
-- uniq list@(x:xs)
--   | x `elem` xs = uniq xs
--   | otherwise = x : uniq xs
