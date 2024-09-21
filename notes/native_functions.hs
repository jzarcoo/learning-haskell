-- Regresa el elemento más grande en una lista
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Lista vacía"
maximum' [x] = x
maximum' (x:xs)
 | x > maxTail = x
 | otherwise = maxTail
   where
     maxTail = maximum' xs

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "Lista vacía"
maximum'' [x] = x
maximum'' (x:xs) = x `max` maximum'' xs

-- Regresa el elemento más pequeño en una lista
minimum' :: (Ord a) => [a] -> a
minimum' [] = error "Lista vacía"
minimum' [x] = x
minimum' (x:xs)
 | x < minTail = x
 | otherwise = minTail
 where
   minTail = minimum' xs

minimum'' :: (Ord a) => [a] -> a
minimum'' [] = error "Lista vacía"
minimum'' [x] = x
minimum'' (x:xs) = x `min` minimum'' xs

-- Toma un Int y algún elemento y devuelve una lista con repeticiones de ese elemento
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' 0 _ = [] -- n <= 0
replicate' n elem = elem : replicate' (n - 1) elem

-- Toma un Int y una lista y regresa los primeros elementos de una lista
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
 | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- Infinito
repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (a:as) (b:bs) = (a,b) : zip' as bs

-- It isn't in-place
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let
    smallerSorted = quicksort [a | a<-xs, a<=x]
    biggerSorted = quicksort [a | a<-xs, a>x]
  in
    smallerSorted ++ [x] ++ biggerSorted

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs) 
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

-- Find the largest number under 100,000 that's divisible by 3829
largestDivisible :: (Integral a) => a
largestDivisible = head (filter (\x -> (x `mod` 3829) == 0) [100000..])

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs)
 | p x = x : takeWhile' p xs
 | otherwise = []


-- We take a natural number.
-- If that number is even, we divide it by two. If it's odd, we multiply it by 3 and then add 1 to that.
-- We take the resulting number and apply the same thing to it, which produces a new number and so on.
-- In essence, we get a chain of numbers.
-- It is thought that for all starting numbers, the chains finish at the number 1.
collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
 | even n = n : collatz (n `div` 2)
 | odd n = n : collatz (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter (\x -> length x > 15) (map collatz [1..100]))

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
