--  ▪ Defina uma função nAnd::Bool->Bool->Bool que gera
-- como resultado True, exceto quando ambos os
-- argumentos são True

nAnd::Bool->Bool->Bool
nAnd a b
    | a && b = False
    | otherwise = True

result :: Bool
result = nAnd False True

main :: IO()
main = putStr (show result)
