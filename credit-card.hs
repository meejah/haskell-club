
toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n
  | n < 10 = [n]
  | otherwise = [mod n 10] ++ toDigitsRev (div n 10)
