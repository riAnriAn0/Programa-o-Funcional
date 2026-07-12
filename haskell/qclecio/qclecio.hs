-- 1. Escreva uma função polimórfica chamada mapeiaCondicional que receba como parâmetro uma
-- função de retorno booleano e duas outras funções, além de uma lista de um tipo qualquer. Para
-- cada elemento da lista, se a primeira função retornar True o elemento deve ser transformado pela
-- segunda função, caso contrário deve ser transformado pela terceira função.
-- Exemplo:
-- mapeiaCondicional even (*2) (+1) [1,2,3,4] = [2,4,4,8]

mapeiaCondicional::(u->Bool)->(u->t)->(u->t)->[u]->[t]
mapeiaCondicional _ _ _ [] = []
mapeiaCondicional f1 f2 f3 (x:xs)
    | f1 x = f2 x:mapeiaCondicional f1 f2 f3 xs
    | otherwise = f3 x: mapeiaCondicional f1 f2 f3 xs

-- 2. Escreva uma função polimórfica chamada combinaAteMenor que receba como parâmetro uma
-- função binária e duas listas de tipos quaisquer. A função deve combinar as listas elemento a
-- elemento até que a menor delas se esgote.
-- Exemplo:
-- combinaAteMenor (+) [1,2,3] [10,20] = [11,22]

combinaAteMenor::(u->u->t)->[u]->[u]->[t]
combinaAteMenor _ [] _ = []
combinaAteMenor _ _ [] = []
combinaAteMenor f (a:as) (b:bs) = f a b : combinaAteMenor f as bs

-- 3. Escreva uma função polimórfica chamada contaSatisfazendoAninhada que receba como
-- parâmetro uma função de retorno booleano e uma lista de listas de um tipo qualquer. A função deve
-- retornar a quantidade total de elementos, dentre todas as sublistas, que satisfazem a condição.
-- Exemplo:
-- contaSatisfazendoAninhada even [[1,2,3],[4,5],[6]] = 3

contaSatisfazendoAninhada::(t->Bool)->[[t]]->Int
contaSatisfazendoAninhada _ [] = 0
contaSatisfazendoAninhada f l  = length [x | x <- l, y <- x, f y]

-- 4. Escreva uma função polimórfica chamada particiona que receba como parâmetro uma função de
-- retorno booleano e uma lista de um tipo qualquer. A função deve retornar uma tupla contendo, na
-- primeira posição, os elementos que satisfazem a condição e, na segunda posição, os elementos
-- que não satisfazem, mantendo a ordem original.
-- Exemplo:
-- particiona even [1,2,3,4,5,6] = ([2,4,6],[1,3,5])

particiona::(t->Bool)->[t]->([t],[t])
particiona _ [] = ([],[])
particiona f l = ([x| x <- l, f x],[y| y<-l, not (f y)])

-- 5. Escreva uma função polimórfica chamada removeAdjacentesIguais que receba como parâmetro
-- uma lista de um tipo qualquer. A função deve remover elementos duplicados que estejam lado a
-- lado, mantendo apenas a primeira ocorrência de cada sequência.
-- Exemplo:
-- removeAdjacentesIguais "aabbbcca" = "abca"

removeAdjacentesIguais::(Eq t)=>[t]->[t]
removeAdjacentesIguais [] = []
removeAdjacentesIguais [x] = [x]
removeAdjacentesIguais (x:y:xs)
    | x == y = removeAdjacentesIguais (y:xs)
    | otherwise = x:removeAdjacentesIguais (y:xs)

-- 6. Escreva uma funçãPo polimórfica chamada iteraAte que receba como parâmetro uma função, uma
-- função de retorno booleano e um valor inicial de um tipo qualquer. A função deve aplicar
-- repetidamente a primeira função ao valor, acumulando cada resultado em uma lista, até que o
-- resultado satisfaça a condição (o elemento que satisfaz a condição deve ser incluído na lista).
-- Exemplo:
-- iteraAte (*2) (>20) 1 = [1,2,4,8,16,32]


iteraAte::(t->t)->(t->Bool)->t->[t]
iteraAte func cond n
    | cond n = [n]
    | otherwise = n:iteraAte func cond (func n)