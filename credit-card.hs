
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
doubleEveryOther (a:b:rest) = [a, b * 2] ++ (doubleEveryOther rest)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (a:as)
  | a < 10 = a + (sumDigits as)
  | otherwise = (sumDigits (toDigits a)) + (sumDigits as)

-- sumDigits appears correct, but .. this doesn't work
validate :: Integer -> Bool
validate x = if (mod (sumDigits (doubleEveryOther (toDigitsRev x))) 10) == 0 then True else False


main = do
     putStrLn $ "foo: " ++ show (toDigitsRev 1234)
     putStrLn $ "foo: " ++ show (toDigits 1234)
     putStrLn $ "foo: " ++ show (doubleEveryOther (toDigits 1234))
     putStrLn $ "foo: " ++ show (sumDigits (doubleEveryOther (toDigits 1234)))
     putStrLn $ "foo: " ++ show (validate 4012888888881881)
     putStrLn $ "foo: " ++ show (validate 4012888888881882)
