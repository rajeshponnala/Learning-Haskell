
sumUpto :: Int -> Int
sumUpto n = sum [1..n]

map1 :: (a -> b) -> [a] -> [b]
map1 f  xs = [ f e | e <- xs ]

map2 :: (a -> b) -> [a] -> [b]
map2 _ [] = []
map2 f (x:xs) = f x : map2 f xs

map3 :: (a -> b) -> [a] -> [b]
map3 _ [] = []
map3 f xs = loop f xs []
    where  loop f (x:[]) acc =  reverse (f x : acc)
           loop f (x:xs) acc = loop f xs (f x : acc)

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 f xs = [ x | x <- xs, f x]

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 _ [] = []
filter2 f (x:xs)
    | f x = x : filter2 f xs
    | otherwise = filter2 f xs

exists :: Eq a => a -> [a] -> Bool
exists _ [] = False
exists d (x:xs)
   | d == x = True
   | otherwise = exists d xs


forAll :: Ord a => ( a -> Bool) -> [a] -> Bool
forAll _ [] = False
forAll f [x] = f x
forAll f (x:xs)
   | f x = forAll f xs
   | otherwise = False

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) =  reverseList xs ++ [x]

reverseListUsingFoldLeft :: [a] -> [a]
reverseListUsingFoldLeft xs = foldl (flip(:)) [] xs

pow :: Int -> Int -> Int
pow x y = product $ replicate y x
