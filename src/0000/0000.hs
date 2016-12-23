main :: IO ()
main = let arr = [1..9]
       in mapM_ putStrLn $ map qq [(x, y)| x <- arr, y <- arr]

qq :: (Integer, Integer) -> String
qq (a, b) = show a ++ "x" ++ show b ++ "=" ++ show (a * b)
