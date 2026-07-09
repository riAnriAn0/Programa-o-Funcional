l :: [Int]
l = [1,2,3,4,5,6,7,8,9,10]

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

valorMin::(Int->Int)->Int->Int
valorMin f n = minimum (map f [0..n])


    -- ▪Teste se o resultado de uma função f aplicada sobre as entradas de 0 a n são todas iguais

isAllEqual :: (Int -> Int) -> Int -> Bool
isAllEqual f n = all (== first) resultados
  where
    resultados = map f [0..n]
    first   = head resultados


    -- ▪Teste se todos os valores de f aplicada às entradas de 0 a n são maiores ou iguais a 0

isAllMaiorIgualZero::(Int->Int)->Int->Bool
isAllMaiorIgualZero f n = all ( >= 0) result
        where
            result = map f [0..n]

    -- ▪Teste se todos os valores de f aplicada às entradas de 0 a n estão em ordem crescente

isCrescente::[Int]->Bool
isCrescente [] = True
isCrescente [x] = True
isCrescente (x:y:xs)
    | y >= x = isCrescente (y:xs)
    | otherwise = False

isOrdemCrescente::(Int->Int)->Int->Bool
isOrdemCrescente f n = isCrescente result
    where
        result = map f [0..n]

aplicacaoExclusiva::(t->Bool)->(t->t->u)->[t]->[t]->[u]
aplicacaoExclusiva f1 f2 l1 l2 = [f2 e1 e2 | e1 <- filter f1 l1, e2 <- filter f1 l2]

contaVizinhosIguais::(Eq t) => [t]->Int
contaVizinhosIguais [] = 0
contaVizinhosIguais [a] = 0
contaVizinhosIguais (a:xs)
    | a == head xs = 1 + contaVizinhosIguais xs
    | otherwise = contaVizinhosIguais xs

contador::(a->Bool)->[a]->Int
contador f l = length (filter f l)

filtraPar::(t->Bool)->(u->Bool)->[t]->[u]->[(t,u)]
filtraPar f g l1 l2 = [(x,y) | x <- filter f l1 , y <- filter g l2]


selecionaExecuta::(t->Bool)->(t->u)->[t]->[u]
selecionaExecuta f g l = map g (filter f l)
