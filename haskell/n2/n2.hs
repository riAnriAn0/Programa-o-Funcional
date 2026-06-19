-- q1

aux::(Char,Int)->String
aux (a,b) 
    | b > 0 =  a :aux (a, b-1)
    | otherwise = " "

descompata::[(Char, Int)]->String
descompata [] = " "
descompata (x:xs) = aux x ++ descompata xs


-- q2

isAmigavel::(Int,Int)->Bool
isAmigavel (a,b)
    | somaA == b && somaB == a = True
    | otherwise = False
    where
        somaA = sum [x| x <- [1..(a-1)], mod a x == 0]
        somaB = sum [y| y <- [1..(b-1)], mod b y == 0]

contaAmigavel::[(Int,Int)]->Int
contaAmigavel [] = 0
contaAmigavel (x:xs) 
    | isAmigavel x = 1 + contaAmigavel xs
    | otherwise = contaAmigavel xs