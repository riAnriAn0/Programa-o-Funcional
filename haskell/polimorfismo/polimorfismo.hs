zip1::[t]->[u]->[(t,u)]
zip1 [] b = []
zip1 a [] = []
zip1 a b = [(x, y)| x <- a, y <- b]

first::(t,u) -> t
first (x,y) = x
