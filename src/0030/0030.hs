import           Control.Monad

main :: IO ()
main =
  getContents >>=
  mapM_
    (print .
     length .
     (\(lists, s) -> filter (\l -> sum l == s) lists) .
     (\[n, s] ->
        ( filter (\l -> length l == n) $ filterM (\_ -> [True, False]) [0 .. 9]
        , s))) .
  takeWhile (\[n, s] -> [n, s] /= [0, 0]) .
  map (map (read :: String -> Int) . words) . lines
