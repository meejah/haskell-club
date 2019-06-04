
type Peg = String
type Move = (Peg, Peg)
hannoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hannoi 1 a b c = [(a, b)]
hannoi n a b c = (hannoi (n - 1) a c b) ++ [(a, b)] ++ (hannoi (n - 1) c b a)


main = do
     putStrLn $ "foo: " ++ show (hannoi 2 "a" "b" "c")
     putStrLn $ "foo: " ++ show (hannoi 3 "a" "b" "c")
