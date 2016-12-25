import GHC.Float.RealFracMethods
import System.IO

main :: IO ()
main = getLine >>= mapM_ (print . double2Int . (`getCompoundInterest` 100000) . (read :: String -> Integer)) . lines

getCompoundInterest :: Integer -> Double -> Double
getCompoundInterest times cost
  | times == 0 = cost
  | times >= 1  = getCompoundInterest (times - 1) (roundUp (cost * interest))
  where interest = 1.05

roundUp :: Double -> Double
roundUp num = int2Double . (* carry_to) . (\(a,b) -> if b /= 0 then a + 1 else a) $ properFraction (num / int2Double carry_to)
  where carry_to = 1000

