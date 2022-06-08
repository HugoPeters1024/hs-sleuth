module Examples where

map :: (a -> b) -> [a] -> [b]
map f as = [ f a | a <- as ]

concat :: [[a]] -> [a]
concat ass = [ a | as <- ass, a <- as ]

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f as = [ b | a <- as, b <- f a ]

filter :: (a -> Bool) -> [a] -> [a]
filter p as = [ a | a <- as, p a ]

prod xs ys = [ x * y | x <- xs, y <- ys ]

foo xs ys = [ x * y | Left (x, _) <- xs, y <- ys ]

bar xs ys zs = [ x * y | x <- xs, y <- ys, z <- zs ]
