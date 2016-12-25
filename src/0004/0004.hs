import Text.Printf

main :: IO ()
main = getContents >>= mapM_ ((\[a, b] -> printf "%.3f %.3f\n" a b) . map (\num -> if num == 0 then 0 else num) . (\[a, b, c, d, e, f] -> [(c * e - b * f) / (a * e - b * d), (a * f - c * d) / (a * e - b * d)]) . map (read :: String -> Double) . words) . lines
