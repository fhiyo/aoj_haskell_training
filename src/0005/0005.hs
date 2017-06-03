main :: IO ()
main =
  getContents >>=
  mapM_
    (putStrLn .
     (\[a, b] -> show a ++ " " ++ show b) .
     (\[a, b] -> [gcd a b, lcm a b]) .
     (\[a, b] ->
        if a < b
          then [b, a]
          else [a, b]) .
     map (read :: String -> Integer) . words) .
  lines
