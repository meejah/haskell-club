
toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n
  | n < 10 = [n]
  | otherwise = [mod n 10] ++ toDigitsRev (div n 10)

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n
  | n < 10 = [n]
  | otherwise = (toDigits (div n 10)) ++ [mod n 10]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [n] = [n]
doubleEveryOther [a, b] = [a * 2, b]
doubleEveryOther (a:b:rest) = [a * 2, b] ++ (doubleEveryOther rest)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [a] = a
sumDigits (a:as) = a + (sumDigits as)

main = do
     putStrLn $ "foo: " ++ show (toDigitsRev 1234)
     putStrLn $ "foo: " ++ show (toDigits 1234)
     putStrLn $ "foo: " ++ show (doubleEveryOther (toDigits 1234))
     putStrLn $ "foo: " ++ show (sumDigits (doubleEveryOther (toDigits 1234)))
