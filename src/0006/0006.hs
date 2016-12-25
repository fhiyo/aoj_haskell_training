main :: IO ()
main = getLine >>= putStrLn . reverse
