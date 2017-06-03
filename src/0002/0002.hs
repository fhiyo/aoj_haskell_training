main :: IO ()
main =
  getContents >>=
  mapM_
    (print .
     length .
     show . (\[a, b] -> a + b) . map (read :: String -> Integer) . words) .
  lines
