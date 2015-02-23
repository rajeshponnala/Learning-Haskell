isPrime :: Int -> Bool
isPrime n =  not $ any(\x ->  n `mod` x == 0) [2..(n `div` 2)]

nPrimes ::  Int -> [Int]
nPrimes n =  take n $ filter isPrime  [2..]

dupNTimes :: Int -> [a] -> [a]
dupNTimes n xs = concat $ [ replicate n x | x <-xs]

span' :: (Ord a) => (a -> Bool) -> [a] -> ([a],[a])
span' f xs =
  let takeList = [ x | x <- xs , f x ]
      dropList = [ x | x <- xs , not $ f x  ]
  in  (takeList,dropList)

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates [x] = [x]
removeDuplicates (x:xs)
     | x == head xs = removeDuplicates xs
     | otherwise = x:removeDuplicates  xs

dropKthElement :: Int -> [a] -> [a]
dropKthElement _ [] = []
dropKthElement 0 xs = xs
dropKthElement 1 (x:xs) = xs
dropKthElement n (x:xs)
     | n < 0 = error "invalid Position"
     | otherwise = x:dropKthElement (n-1) xs
