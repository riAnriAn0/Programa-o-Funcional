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
formTupla lista = [(contPalavra a (lista), a)]++formTupla [x | x <- lista, x /= a]
    where 
        a = head lista

ordenaSort::[(Int, String)]->[String]
ordenaSort [] = []
ordenaSort lista = (ordenaSort p1) ++ [snd p0] ++ (ordenaSort p2)
    where 
        p0 = head lista
        p1 = [ x | x <- tail lista, (fst x ) >= (fst p0)]
        p2 = [ x | x <- tail lista, (fst x ) < (fst p0)]

ordena::[String]->[String]
ordena [] = []
ordena lista = ordenaSort ( formTupla lista)
