--  ▪Defina uma função numEquallMax::Int->Int->Int->Int
-- onde numEquallMax n m p retorna a quantidade de
-- números iguais ao máximo entre n, m e p

nMax::Int -> Int -> Int -> Int
nMax a b c
    | a >= b && a >= c = a
    | b >= a && b >= c = b
    | otherwise = c

numEquallMax::Int->Int->Int->Int
numEquallMax n m p
    | n == maximo && m == maximo && p == maximo = 3
    | n == maximo && m == maximo = 2
    | n == maximo && p == maximo = 2
    | m == maximo && p == maximo = 2
    | otherwise = 1
    where maximo = nMax n m p

result :: Int
result = numEquallMax 7 2 7

main :: IO()
main = putStr (show result)
