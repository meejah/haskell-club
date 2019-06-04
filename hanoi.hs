
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi n a b c = (hanoi (n - 1) a c b) ++ [(a, b)] ++ (hanoi (n - 1) c b a)


main = do
     putStrLn $ "foo: " ++ show (hanoi 2 "a" "b" "c")
     putStrLn $ "foo: " ++ show (hanoi 3 "a" "b" "c")
