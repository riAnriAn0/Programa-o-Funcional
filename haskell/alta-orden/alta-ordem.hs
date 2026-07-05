l::[Int] = [1,2,3,4,5,6,7,8,9,10]

-- ▪Defina funções que tomem uma lista de inteiros L e

    -- ▪Retorne a lista dos quadrados dos elementos de L

listaL::[Int]->[Int]
listaL = map (^2)

    -- ▪Retorne a soma dos quadrados dos elementos de L

somaQuadrado:: [Int] -> Int
somaQuadrado n = sum (listaL n)

    -- ▪Verifique se todos os elementos da lista são positivos

isPositivo::[Int]->Bool
isPositivo = all (>0) 

-- ▪Escreva funções que calculem

    -- ▪O valor mínimo de uma função aplicada a uma lista de inteiros de 0 a n  

valorMin::([Int]->[Int])->[Int]->Int
valorMin _ [] = 0
valorMin f l = 


    -- ▪Teste se o resultado de uma função f aplicada sobre as entradas de 0 a n são todas iguais

    -- ▪Teste se todos os valores de f aplicada às entradas de 0 a n são maiores ou iguais a 0

    -- ▪Teste se todos os valores de f aplicada às entradas de 0 a n estão em ordem crescente