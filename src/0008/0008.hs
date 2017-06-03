-- XXX: I referred to other people's answers.
-- From: http://judge.u-aizu.ac.jp/onlinejudge/review.jsp?rid=1500617#1
main :: IO ()
main = getContents >>= mapM_ (print . length . solve . read) . lines

solve :: Int -> [(Int, Int, Int)]
solve n =
  [ (a, b, c)
  | a <- [0 .. 9]
  , b <- [0 .. 9]
  , c <- [0 .. 9]
  , let d = n - a - b - c
  , 0 <= d && d <= 9
  ]
