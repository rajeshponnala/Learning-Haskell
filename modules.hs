import Data.List
import Data.Char


qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where smaller=[a | a <- xs, a <= x]
                     larger =[a | a <- xs, a > x ]

qsort' :: (Ord a) => [a] -> [a]
qsort' [] = []
qsort' (x:xs) = 
	let smallerOrEqual = [a | a <- xs, a <= x]
	    larger= [a | a <- xs, a > x]
	in qsort' smallerOrEqual ++ [x] ++ qsort' larger


productOfList :: (Num a) => [a] -> a 
productOfList [] = 1
productOfList (x:xs) = x * productOfList xs

qsortReverse' :: (Ord a) => [a] -> [a]
qsortReverse' [] = []
qsortReverse' (x:xs) = 
	let smallerOrEqual = [a | a <- xs, a <= x]
	    larger= [a | a <- xs, a > x]
	in qsortReverse' larger ++ [x] ++ qsortReverse' smallerOrEqual


wordNums :: String -> [(String,Int)]
wordNums = map(\ws -> (head ws,length ws)).group.sort.words 

encode :: Int -> String -> String
encode offset msg = map(\c -> chr $ ord c + offset) msg

decode :: Int -> String -> String 
decode shift msg = encode (negate shift) msg

digitSum :: Int -> Int
digitSum = sum.map digitToInt.show

firstToN  :: Int -> Maybe Int
firstToN n = find(\x -> digitSum x ==n) [1..]