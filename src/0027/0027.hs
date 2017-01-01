import Data.List

main :: IO ()
main = getContents >>= mapM_ (putStrLn . get2004WDay) . takeWhile (\[m, _] -> m /= 0) . map (map (read :: String -> Int) . words) . lines

get2004WDay :: [Int] -> String
get2004WDay [month, day]
  | wday == 1 = "Monday"
  | wday == 2 = "Tuesday"
  | wday == 3 = "Wednesday"
  | wday == 4 = "Thursday"
  | wday == 5 = "Friday"
  | wday == 6 = "Saturday"
  | wday == 7 = "Sunday"
  where wday = getDayOfTheWeekOf2004 month day

getDayOfTheWeekOf2004 :: Int -> Int -> Int
getDayOfTheWeekOf2004 month day = (sum (take (month - 1) days) + day + 2) `mod` 7 + 1
  where
    days = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

