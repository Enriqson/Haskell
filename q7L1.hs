mul2:: [Int]->[Int]->[Int]

mul2 [] [] = []
mul2 [] (b:bs) = 0 : (mul2 [] bs)
mul2 (a:as) [] = 0 : (mul2 as [])
mul2 (a:as) (b:bs) = (a*b): mul2 as bs