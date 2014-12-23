import Data.Char (digitToInt)
binary :: Int -> String
binary 0 = "0"
binary 1 = "1"
binary x = binary (x `quot` 2) ++ show (x `rem` 2)

data Binary = Zero | One


decimal :: String -> Int
decimal xs = foldl (\prevSum current -> 2 * prevSum + current) 0 (map digitToInt xs)

main = do 
  print $ (binary . decimal) "10011011100"
  print $ (decimal . binary) 101311311
  print $ binary 12
  print $ decimal "1100"
