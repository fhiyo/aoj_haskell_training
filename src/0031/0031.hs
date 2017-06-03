import           Data.Char
import           Data.List

main :: IO ()
main =
  getContents >>=
  mapM_
    (putStrLn .
     unwords .
     map fst .
     filter (\(a, b) -> b == 1) .
     zip ["1", "2", "4", "8", "16", "32", "64", "128", "256", "512"] .
     map digitToInt . reverse . i2b . (read :: String -> Int)) .
  lines

-- Reference from: http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=0031
b2i :: String -> Int
b2i xs = foldl (\x y -> 2 * x + y) 0 $ map (\x -> read [x]) xs

i2b :: Int -> String
i2b = concatMap show . reverse . i2b'
  where
    i2b' 0 = []
    i2b' n = mod n 2 : i2b' (div n 2)
