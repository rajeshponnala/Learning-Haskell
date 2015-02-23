
doubleMe x = x + x

doubleUs x y=  doubleMe x  + doubleMe y

doubleSmallNumber x = if x > 100 then x else doubleMe

conanO'Brien = "It's a-me, Conan O'Brien!"

multiplyBy5 range= [ x * 5 | x <- [1..range]]

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

len xs = sum[1 | _ <- xs]

removeNonUpperCase st = [ c | c <- st, c `elem` ['A'..'Z'] ]

nestedListComprehension xxs = [[ x | x <- xs, even x ] | xs <- xxs]

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
	| even n = n:chain( n `div` 2)
	| odd  n = n: chain ( n * 3 +1)

sum' :: (Num a) => [a] -> a
sum' = foldl(+) 0

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
   where (f:_) = firstname
         (l:_) = lastname
