zip1 :: [a] -> [b] -> [(a,b)]
zip1 [] _ = []
zip1 _ [] = []
zip1 (x:xs)(y:ys) = (x,y): zip1 xs ys


zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' xs ys zs =  map(\((a,b),c) -> (a,b,c)) $  (xs `zip1` ys `zip1` zs) 
