import Data.List

myLast :: [a] -> a
myLast [] = error "No end for empty lists!"
myLast [x]= x
myLast (_:xs) = myLast xs

myLast' :: [a] -> a
myLast' xs = head $ reverse xs

myButLast :: [a] -> a
myButLast xs = head $ tail $ reverse xs

myButLast' :: [a] -> a
myButLast' [] = error "No elements in List"
myButLast' [_] = error "Single element List"
myButLast' [x,_] = x
myButLast' (_:xs) = myButLast' xs

elementAt' :: [a]-> Int -> a
elementAt' (x:_) 1 = x
elementAt' [] _ = error "Index out of bounds"
elementAt' (_:xs) k
    | k < 1 = error "Invalid Position"
    | otherwise = elementAt' xs (k-1)


myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myLength' :: [a] -> Int
myLength' xs = foldl (\acc _ -> acc+1) 0 xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = False
isPalindrome xs =  reverse xs == xs

duplicateList :: [a] -> [a]
duplicateList xs = foldr (\e acc -> e:e:acc) [] xs

duplicateList' :: [a] -> [a]
duplicateList' [] = []
duplicateList' (x:xs) = x:x:duplicateList' xs
