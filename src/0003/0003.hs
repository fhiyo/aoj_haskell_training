import Data.List

main :: IO ()
main = getContents >>= mapM_ (putStrLn . (\[a, b, c] -> if a ^ 2 + b ^ 2 == c ^ 2 then "YES" else "NO") . sort . map (read :: String -> Integer)) . tail . map words . lines
