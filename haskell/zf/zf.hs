l::[Int]
l = [1,3,2,5,1,7]

r = [2*a | a <- l]

p = [a | a<-l , mod a 2 ==0]

-- quickSort com ZF
quickSort::[Int]->[Int]
quickSort [] = []
quickSort (a:xs) = quickSort x1 ++ [a] ++ quickSort x2
    where
        x1 = quickSort [y | y <- xs, y <= a]
        x2 = quickSort [y | y <- xs, y > a] 
        

-- ▪Fatores - Lista os divisores de um número
fatores::Int->[Int]
fatores 0 = [] 
fatores n = [x | x <- [1..(n-1)] , mod n x == 0]

-- ▪Perfeitos - Lista de números perfeitos de 1 até um número inteiro informado
nPerft::Int->[Int]
nPerft 0 = [0]
nPerft n = [x | x <- [1..n], sum (fatores x) == x ] 

-- ▪Concatena - Semelhante a função concat (concatena uma lista de listas em uma única)
