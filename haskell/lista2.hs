-- 11. Implemente a função 'split', que recebe um número inteiro n e uma
-- lista de números inteiros e retorna uma tupla onde o primeiro
-- elemento é uma lista dos itens maiores que n e o segundo
-- elemento é uma lista dos itens menores.
-- Exemplo:
-- split 5 -> [1,2,3,4,5,6,7,8] -> ([6,7,8],[1,2,3,4,5])

splite::Int->[Int]->([Int],[Int])
splite _ [] = ([],[])
splite n lista = (maiores, menores)
    where
        maiores = [x | x <- lista , x >= n ]
        menores = [x | x <- lista , x < n ]

-- 12. Escreva uma função que dados dois índices, m e n, extraia da
-- lista os elementos compreendidos entre entre esses valores, onde
-- ambos os limites estão incluídos. Comece a contar os elementos
-- do 1.
-- Exemplo:
-- ['a','b','c','d','e','f','g','h','i','k'] 3 7 -> "cdefg"

extrai::Int->Int->[u]->[u]
extrai _ _ [] = []
extrai n m lista = drop (n-1) (take m lista)
