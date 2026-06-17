import Prelude

-- questão 1
pertenceLista::Int->[Int]->Bool
pertenceLista n lista
    | null lista = False
    | n == head lista = True
    | otherwise = pertenceLista n (tail lista)

-- questão 2 

maiorElemento :: [Int] -> Int
maiorElemento [] = error "Lista vazia"
maiorElemento [x] = x
maiorElemento (x:xs)
    | maiorElemento xs >= x = maiorElemento xs
    | otherwise = x


-- quetão 3
posicao::Int->[Char]->Char
posicao i l
    | null l = '*'
    | i == 0  = head l
    | otherwise = posicao (i-1) (tail l)


-- Escreva uma função que retire o n-ésimo elemento de uma lista. Exemplo:"abcdefghi" 3 -> "abdefghi"

removeN :: [Int] -> Int -> [Int]
removeN [] _ = error "lista vazia"
removeN lista n = take (n-1) lista ++ drop n lista

-- Escreva uma função que receba uma frase e diga se esta é um palíndromo
-- Para verificar se uma frase é palíndromo basta verificar se ela é igual à sua reversa (implemente a função reverse)

aux :: [a] -> [a]
aux []     = []                    
aux (x:xs) = aux xs ++ [x]  

isPalindromo::[Char]->Bool
isPalindromo [] = False
isPalindromo lista 
    | lista == aux lista = True
    | otherwise = False


-- Escreva uma função que elimina caracteres repetidos. Eles devem ser substituídos por uma única cópia do elemento e a ordem dos elementos não deve ser alterada.
-- Ex.: "aaaabccaadeeee" -> "abcade"

noRepite :: [Char] -> [Char]
noRepite []  = []
noRepite [x] = [x]
noRepite (x:y:resto)
    | x == y    = noRepite (y:resto)
    | otherwise = x : noRepite (y:resto)

-- 7. Escreva uma função que duplique cada elemento de uma lista.
-- Exemplo:
-- [1, 2, 3] -> [1,1,2,2,3,3]

duplica::[Int]->[Int]
duplica [] = []
duplica (x:xs) = x:x:duplica xs 

-- 8. Escreva uma função que recebe uma String e retorna a primeira
-- palavra dessa String sem contar pontuação.
-- Exemplo:
-- “Olá, mundo” -> “Olá”

palavra::[Char]->[Char]
palavra [] = []
palavra (x:xs) 
    | x == ' ' = []
    | x /= ',' && x /= '.' && x /= ';' =  x:palavra xs
    | otherwise = x:palavra xs

-- 9. Implemente uma função que move todos os elementos de uma
-- lista para a direita.
-- Exemplo:
-- moverDireita [ 'a', 'b', 'c' ] 0 -> [ 'a', 'b', 'c' ]
-- moverDireita [ 'a', 'b', 'c' ] 1 -> [ 'c', 'a', 'b' ]
-- moverDireita [ 'a', 'b', 'c' ] 2 -> ['b', 'c', 'a']

moverDireita::[Int]->Int->[Int]
moverDireita [] _ = []
moverDireita lista p = drop ((length lista) - p) lista ++ take ((length lista) - p) lista


-- 10 . Implemente uma função que recebe duas listas sem elementos
-- repetidos e retorna uma lista com elementos comuns entre elas.
-- Exemplo:
-- intercede [1,2,3,4] [2,3,4,5] -> [2,3,4]

intercede::[Int]->[Int]->[Int]
intercede [] [] = []
intercede l1 l2 = [x | x <- l1 , y <- l2 , x == y]

-- 11. Implemente a função 'split', que recebe um número inteiro n e uma
-- lista de números inteiros e retorna uma tupla onde o primeiro
-- elemento é uma lista dos itens maiores que n e o segundo
-- elemento é uma lista dos itens menores.
-- Exemplo:
-- split 5 -> [1,2,3,4,5,6,7,8] -> ([6,7,8],[1,2,3,4,5])

split::Int->[Int]->([Int],[Int])
split _ [] = ([], [])
split n lista = (l1, l2)
    where 
        l1 = [x | x <- lista , x > n]
        l2 = [x | x <- lista , x < n]

-- 20. Considere que o preço de uma passagem de avião em um trecho
-- pode variar dependendo da idade do passageiro. 
-- Pessoas com 60 anos ou mais pagam apenas 60% do preço total. 
-- Crianças até 10 anos pagam 50% 
-- Bebês (abaixo de 2 anos) pagam apenas 10%.
-- Elabore uma função que tenha como entrada uma lista de tuplas
-- composta pelo valor da passagem e a idade do passageiro,
-- respectivamente, e produza o valor total a ser pago.

valorPasagem::[(Float, Int)]->Float
valorPasagem [] = 0.0
valorPasagem (x:xs)
    | snd x < 2 = (0.1 * fst x) + valorPasagem xs
    | snd x <= 10 = (0.5 * fst x) + valorPasagem xs
    | snd x >= 60 = (0.6 * fst x) + valorPasagem xs
    | otherwise = fst x + valorPasagem xs

-- 4.Um trio (x, y, z) de inteiros positivos diz-se pitagórico se
-- . Defina a função 𝑥² + 𝑦² = 𝑧²
-- pitagóricos ∶∶ Integer → [(Integer, Integer, Integer)]
-- que calcule todos os trios pitagóricos cujas componentes não
-- ultrapassem o argumento. Por exemplo: pitagóricos 10 = [(3, 4, 5), (4,
-- 3, 5), (6, 8, 10), (8, 6, 10)].

pitagoras::Int->[( Int, Int, Int)]
pitagoras n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]


-- 17. Defina, em Haskell, uma função f que, dadas uma lista i de inteiros
-- e uma lista l qualquer, retorne uma nova lista constituı́da pela
-- lista l seguida de seus elementos que têm posição indicada na
-- lista i, conforme o exemplo a seguir:
-- f [2,1,4] [’a’, ’b’, ’c’, ’d’] -> [’a’, ’b’, ’c’, ’d’, ’b’, ’a’, ’d’]

aux2::[Int]->[Char]->[Char]
aux2 [] _ = []
aux2 _ [] = []
aux2 (x:xs) listaC = (listaC!!x):f xs listaC

f::[Int]->[Char]->[Char]
f listaI listaC = listaC : aux2 listaI listaC
