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