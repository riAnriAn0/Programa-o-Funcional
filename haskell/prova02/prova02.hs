-- 1. Imagine  que  você  necessita  realizar  uma  análise  de  dados  transmitidos  ente  dois  dispositivos 
-- eletrônicos  em  que  a  frequência  de  repetição  das  informações  é  o  dado  mais  relevante.  Para 
-- tanto  você  necessita  implementar  uma  função  que  receba  uma  lista  de  strings  e  retorne  uma 
-- lista  com  as  strings  ordenadas  por  frequência  de  aparição  (da  que  mais  se  repete  para  a  que 
-- menos se repete). (5,0) 
-- Exemplo: 
-- [“casa”, “bola”, “casa”, “rua" , “rua”, "casa"]  -> [“casa”, “rua”, “bola"] 

contPalavra::String->[String]->Int
contPalavra s [] = 0
contPalavra s  (a:xs)
    |  s == a = 1 + contPalavra s xs
    | otherwise = contPalavra s xs

formTupla::[String]->[(Int, String)]
formTupla [] = []
formTupla lista = (contPalavra a lista, a) : formTupla [x | x <- lista, x /= a]
    where
        a = head lista

ordenaSort::[(Int, String)]->[String]
ordenaSort [] = []
ordenaSort lista = ordenaSort p1 ++ [snd p0] ++ ordenaSort p2
    where
        p0 = head lista
        p1 = [ x | x <- tail lista, fst x >= fst p0]
        p2 = [ x | x <- tail lista, fst x  < fst p0]

ordena::[String]->[String]
ordena [] = []
ordena lista = ordenaSort ( formTupla lista)

{- 
2. Na teoria da informação, a distância de Hamming entre duas strings de mesmo comprimento é 
o número de posições nas quais elas diferem entre si. Vista de outra forma, ela corresponde ao 
menor  número  de  substituições  necessárias  para  transformar  uma  string  na  outra,  ou  o 
número  de  erros  que  transformaram  uma  na  outra.  Por  exemplo,  a  distância  de  Hamming 
entre "elabore" e "melhore" é 4, entre "217396" e "22339" é 3. Esta propriedade é 
amplamente  utilizada  em  processadores  de  textos  e  corretores  ortográficos.  Com  base  nisso, 
escreva uma função em Haskell que receba duas listas de strings e retorne uma lista de tuplas 
contendo os pares de string entre as listas e a respectiva distância de Hamming. (5,0) 
Exemplo: 
hamming  ["sol",  "casa"]  [“mola",  "cama"]  ->  [("sol",  "mola",  2),  (“sol",  "cama",  4),  (“casa",  "mola",  3), 
("casa", "cama", 1) 
-}

