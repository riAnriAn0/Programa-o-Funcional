class Located a where
getLocation :: a -> (Int, Int)

data Point = Point { coordX::Int, coordY::Int}

instance Located Point where
getLocation p = (coordX p, coordY p)

origem:: (Located t)=> t -> Bool
origem p = getLocation p == (0,0)
