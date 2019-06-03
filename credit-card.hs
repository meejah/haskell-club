
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n
  | n < 10 = [n]
  | otherwise = [mod n 10] ++ toDigits (div n 10)
