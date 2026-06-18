zip1::[t]->[u]->[(t,u)]
zip1 [] b = []
zip1 a [] = []
zip1 a b = [(x, y)| x <- a, y <- b]

first::(t,u) -> t
first (x,y) = x

-- ▪Defina uma função concat onde concat [e1, ..., ek] = e1 ++ ... ++ ek. Qual o tipo de concat?

concatena::[[u]]->[u]
concatena [] = []
concatena (x:xs) = x++concatena xs

-- ▪Defina uma função unZip que transforma uma lista de pares em um par de listas. Qual o seu tipo?

unZipi::[(t,u)] -> ([t],[u])
unZipi [] = ([],[])
unZipi l = ([fst x| x <- l],[snd x| x <- l])

-- ▪Defina uma função init::[t]->[t] que retorna todos os elementos de uma lista com exceção do último

initi::[t]->[t]


-- ▪Defina as funções tome e tire do tipo Int->[t]->[t] onde tome n l retorna os n primeiros elementos da lista l e  tire n l retira os n primeiros elementos da lista l