reduCond::(t->t->t)->(t->Bool)->[t]->t -- assinatura errada
reduCond func cond l = foldr1 func (filter cond l)


segunda::(Eq t,Eq u)=>[(t,u)]->[t]->[u]
segunda lt (x:xs) = [s | (f,s) <- lt, f == x]++segunda lt xs -- troquei ++ por : 



