<a name="hs"></a>
# Haskell

## Contents

- [Native Functions](#nativas)
  - [Lists](#listas)
    - [Infinite lists](#listas_infinitas)
  - [Tuples](#tuplas)
  - [Higher Order Functions](#nf_higher_order_functions)
  - [Others](#otras)
- [Topics](#temas)
  - [Types and Typeclasses](#tipos_clases)
  - [Recursion](#recursion)
  - [Higher Order Functions](#f_ord_sup)
    - [Currying](#currying)
       - [Partial Application](#partial_application)
    - [High-order](#high_order)
    - [Lambdas](#lambdas)
    - [Folding](#folding)
    - [Function application with $](#pesos)
    - [Function composition](#Function_composition)
  - [Módulos](#modulos)
  
---

<a name="nativas"></a>
## Native Functions

<a name="listas"></a>
### Lists

```hs
-- It takes a predicate and a list and then goes from the beginning of the list and returns its elements while the predicate holds true.
-- Once an element is found for which the predicate doesn't hold, it stops.
takeWhile :: (a -> Bool) -> [a] -> [a]

-- Divide la cadena en palabras utilizando espacios en blanco (espacios, tabulaciones, y saltos de línea) como delimitadores
words :: String -> [String]
-- Toma una lista de palabras y las concatena en una única cadena, separando cada palabra por un espacio en blanco.
unwords :: [String] -> String

-- Toman un predicado y una lista y comprueban si el predicado se satisface para algún o para todos los elementos respectivamente.
all :: Foldable t => (a -> Bool) -> t a -> Bool
any :: Foldable t => (a -> Bool) -> t a -> Bool

-- Máximo
maximum :: (Foldable t, Ord a) => t a -> a

-- Mínimo
minimum :: (Foldable t, Ord a) => t a -> a

-- Longitud. Regresa Int!!
length :: Foldable t => t a -> Int

-- Suma
sum :: (Foldable t, Num a) => t a -> a

-- Producto
product :: (Foldable t, Num a) => t a -> a

-- Contiene
elem :: (Foldable t, Eq a) => a -> t a -> Bool

-- Tomar los primeros n elementos de una lista
take :: Int -> [a] -> [a]

-- Se utiliza para eliminar los primeros n elementos de una lista.
drop :: Int -> [a] -> [a]

 -- Se utiliza para crear una lista que contiene múltiples repeticiones de un mismo valor.
replicate :: Int -> a -> [a]

-- Toma una lista de booleanos y devuelve True solo si todos los elementos de la lista son True.
and :: Foldable t => t Bool -> Bool
-- Devuelve True solo si existe algún elemento True en la lista.
or :: Foldable t => t Bool -> Bool

-- 1er elemento
head :: [a] -> a
-- Sin el 1er elemento
tail :: [a] -> [a]

-- Último elemento
last :: [a] -> a
-- Sin el último elemento
init :: [a] -> [a]

-- Takes a function and a list and applies that function to every element in the list, producing a new list
map :: (a -> b) -> [a] -> [b]

-- Takes a predicate and a list and then returns the list of elements that satisfy the predicate.
filter :: (a -> Bool) -> [a] -> [b]

-- Lista
reverse :: [a] -> [a]

-- Obtener el elemento de una lista sabiendo su índice
(!!) :: [a] -> Int -> a

-- Este operador se utiliza para agregar un elemento al principio de una lista
-- '' char
(:)

-- Este operador se utiliza para concatenar dos listas
-- "" string
(++)
```

<a name="listas_infinitas"></a>
#### Infinite lists

```hs
-- Repite el único elemento
repeat :: a -> [a]

-- Ciclo de listas iguales
cycle :: [a] -> [a]
```

---

<a name="tuplas"></a>
### Tuples

```hs
-- Regresa el 1er elemento
fst :: (a, b) -> a

-- Regresa el 2do elemento
snd :: (a, b) -> b

-- Toma 2 listas y las une en 1, juntando sus elementos en una tupla
zip :: [a] -> [b] -> [(a, b)]
```

---

<a name="nf_higher_order_functions"></a>
## Higher Order Functions

```hs
-- Toma una función binaria y dos listas, y devuelve una lista que resulta de aplicar la función binaria a los elementos correspondientes de las dos listas.
-- Si una de las listas es más corta que la otra, los elementos sobrantes de la lista más larga se ignoran
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

-- It evaluates the function flipping the order of arguments
flip :: (a -> b -> c) -> b -> a -> c

-- Devuelve una lista [b] que resulta de aplicar la función a cada elemento de la lista y luego concatenar los resultados.
concatMap :: (a -> [b]) -> [a] -> [b]

-- Asocia por la derecha/final (\x acc -> ...)
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldr1 :: Foldable t => (a -> a -> a) -> t a -> a

-- Asocia por la izquierda/inicio (\acc x -> ...)
foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldl1 :: Foldable t => (a -> a -> a) -> t a -> a

-- lowest precedence & right-associative.
($) :: (a -> b) -> a -> b

-- Function composition
(.) :: (b -> c) -> (a -> b) -> a -> c
```

---

<a name="otras"></a>
### Others

```hs
-- Convierte un Int a otro más general (útil en length)
fromIntegral :: (Integral a, Num b) => a -> b

-- ¿Es impar?
odd :: Integral a => a -> Bool
-- ¿Es par?
even :: Integral a => a -> Bool

-- Máximo entre 2 elementos
max :: Ord a => a -> a -> a
-- Mínimo entre 2 elementos
min :: Ord a => a -> a -> a

-- Elemento siguiente
succ :: Enum a => a -> a

-- Resta
subtract :: Num a => a -> a -> a

-- División redondea a entero
div :: Integral a => a -> a -> a --div 5 2 = 2

-- División
(/) :: Fractional a => a -> a -> a --5/2=2.5
```

---

<a name="temas"></a>
## Topics

<a name="tipos_clases"></a>
### Types and Typeclasses

```hs
Eq

-- For types that have an ordering.
Ord
-- Takes two Ord members of the same type and returns an ordering: GT, LT or EQ.
compare :: Ord a => a -> a -> Ordering

-- Toma una cadena y devuelve un valor del tipo
read :: Read a => String -> a
-- Anotaciones de tipo. Ejemplo:
read "(3, 'a')" :: (Int, Char)

show :: Show a => a -> String

Enum

Bounded

Num

Integral

Floating
```

---

<a name="recursion"></a>
### Recursion

Implementación de funciones nativas

```hs
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
```

QuickSort
```hs
-- It isn't in-place
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let
    smallerSorted = quicksort [a | a<-xs, a<=x]
    biggerSorted = quicksort [a | a<-xs, a>x]
  in
    smallerSorted ++ [x] ++ biggerSorted
```

---

<a name="f_ord_sup"></a>
### Higher Order Functions

<a name="currying"></a>
#### Currying

Currying is the process of transforming a function that takes multiple arguments in a tuple as its argument, into a function that takes just a **single argument** and **returns another function** which accepts further arguments, one by one, that the original function would receive in the rest of that tuple. 

> All functions in Haskell take just one argument

- It can be said that arrows in the types notation *associate to the right*, so that <code>f :: a -> b -> c</code> is really <code>f :: a -> (b -> c)</code>
- Functional application, correspondingly, *associates to the left*: <code>f x y</code> is really <code>(f x) y</code>

As an illustration, `div` takes an `Int` and returns something of the type `Int -> Int`. So, <code>(div 11) 2</code> yields <code>5</code>

<a name="partial_application"></a>
##### Partial Application

```hs
-- (+) :: Num a => a -> a -> a
let addOne = (+) 1
addOne 10 --11
```
`addOne` is the result of partially applying `(+)`. It is a new function that takes a Num, adds 1 to it and returns that as the result. This means that `(+)` actually takes one argument and returns a function that takes another argument and returns a Num. 

Using partial application (calling functions with too few parameters, if you will) is a neat way to create **functions on the fly** so we can pass them to another function or to seed them with some data.

Functions can also be partially applied by using sections (simply surround it with parentheses and only supply a parameter on one side). That creates a function that takes one parameter and then applies it to the side that's missing an operand.

```hs
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])
```

In JavaScript
```js
const divisionNormal_ArrowFunction = (a,b) => a/b;
const divisionNormal = function (a, b) {
  return a / b;
}
divisionNormal(10,2); //5

const divisionCurry_ArrowFunction = a => b => a/b;
const divisionCurry = function (a) {
  return function(b) {
  	return a / b;
	}
}
divisionCurry(10)(2); //5

const divWithTen = divisionCurry(10);
divWithTen(2); //5
```

---

<a name="high_order"></a>
#### High-order

> A higher-order function is a function that takes other functions as arguments or returns a function as result. 

```hs
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
```

If our function requires us to pass it a function that takes only one parameter, we can just partially apply a function to the point where it takes only one parameter and then pass it. <code>applyTwice (3:) [1] --[3,3,1]</code>
```hs
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

-- We take advantage of the fact that functions are curried
let f = (subtract)
let f_flip = flip f
let f_flip_3 = f_flip 3
f_flip_3 4 -- -1

-- Using map is much more readable than list comprenhension
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

map (++ "!") ["BIFF", "BANG", "POW"] -- ["BIFF!","BANG!","POW!"]

let listOfFuns = map (*) [0..]
(listOfFuns !! 4) 5 -- 20

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs) 
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

filter null [[], [4]] --[]

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs)
 | p x = x : takeWhile' p xs
 | otherwise = []

takeWhile (/=' ') "Hola mundo" -- "Hola"
```

Exercises: 

- Find the sum of all odd squares that are smaller than 10,000
```hs
sum (takeWhile (<10000) (filter odd (map (^2) [1..]))) -- 166650
sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)]) -- 166650
```

- Find the largest number under 100,000 that's divisible by 3829
```hs
largestDivisible :: (Integral a) => a
largestDivisible = head (filter (\x -> (x `mod` 3829) == 0) [100000..])
```

- Collatz sequences. For all starting numbers between 1 and 100, how many chains have a length greater than 15?
```hs
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
```

- Convierte cadena a mayúsculas.
  - `map`
```hs
import Data.Char

upperCase :: String -> String
upperCase s = map toUpper s
```

- Obtiene la sucesión de fibonacci.
  - `map`
```hs
-- Fibonacci
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Sucesión de Fibonacci
sFib :: Int -> [Int]
sFib n = map fib [0..n]
```

- Regresa palíndromos.
  - `filter`
```hs
-- Palíndromas
palindromo :: String -> Bool
palindromo x = x == reverse x

-- Lista palíndromas
palindromas :: [String] -> [String]
palindromas l = filter palindromas l
```

- Elimina ocurrencias del elemento.
  - `filter`
```hs
quitaElemento :: (Eq a) => [a] -> a -> [a]
quitaElemento l e = filter (/= e) l
```

- Quita las listas vacías de una lista de listas
  - `filter`
```hs
wel :: [[a]] -> [[a]]
wel l = filter (not . null) l
```

---

<a name="lambdas"></a>
#### Lambdas

> An anonymous function is a function without a name. `(\x y -> x + y)`

You can **pattern match** in lambdas. THe only difference is that you can't define several patterns for one parameter, like making a `[]` and a `(x:xs)`.

Lambdas are normally *surrounded by parentheses* unless we mean for them to *extend all the way to the right*. Here's something interesting: due to the way functions are curried by default, these two are equivalent:

```hs
addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z

addThree :: (Num a) => a -> a -> a -> a
addThree = \x -> \y -> \z -> x + y + z
```

So use lambdas in this way when you want to make it explicit that your function is mainly meant to be *partially applied* and *passed on to a function as a parameter*.

Exercises:

- Determina cual es positivo, negativo.
  - **lambdas** y `map`
```hs
signoNums :: [Int] -> [String]
signoNums l = map (\x -> if x < 0
  then "negativo"
  else if x > 0
    then "positivo"
    else "cero"
  ) l
```

- Regresa la suma de los primeros n+1 cuadrados.
  - **lambdas**, `sum` y `map`
```hs
sumSqrt :: Int -> Int
sumSqrt n = sum (map (\y -> y^2) [0..n])
```

- Filtra las cadenas más largas.
  - **lambdas**, `filter` y `length`
```hs
tooLong :: [String] -> [String]
tooLong l = filter (\x ->
  length z<= 7) l
```

- Dentro de una cadena, reemplaza las apariciones de un caracter por el segundo.
  - **lambdas** y `map`
```hs
replace :: String -> Char -> Char -> String
replace s c1 c2 = map
  (\cs -> if cs == c1
          then c2
          else cs) s
```

---

<a name="folding"></a>
#### Folding

Folds can be used to implement any function where you traverse a list once, element by element, and then return something based on that. 

> Reduce a data structure to some single value

The `x:xs` pattern is very common

```hs
sum [] = 0 
sum (x:xs) = (+) x (sum xs) 

product [] = 1 
product (x:xs) = (*) x (prod xs) 

concatenar [] = []
concatenar (x:xs) = (++) x (concatenar xs)
```

A fold takes 
1. A binary function
2. A starting value (accumulator)
3. A list to fold up. 

```hs
-- Asocia por la derecha/final (\x acc -> ...)
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

-- Asocia por la izquierda/inicio (\acc x -> ...)
foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
```

Así,

```hs
foldr (\x y -> x + y ) 0 [1..100] --5050
foldr (\x y -> 1 + y) 0 "hola" --4
foldr (\x y -> x^2 + y) 0 [1..5] --55
foldr (:) [6..10] [1..5] --[1,2,3,4,5,6,7,8,9,10]
foldr (\x ys -> 1 : ys) [] [1..5] --[1,1,1,1,1] map (\x->1) [1..5]

foldl (\x ys -> ys : x) [] [1..5] --[5,4,3,2,1]
```

The `foldl1` and `foldr1` functions work much like foldl and foldr, only you don't need to provide them with an explicit starting value. They assume the first (or last) element of the list to be the starting value and then start the fold with the element next to it.

```hs
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)
```

`scanl` and `scanr` are like foldl and foldr, only they report all the intermediate accumulator states in the form of a list. There are also `scanl1` and `scanr1`, which are analogous to foldl1 and foldr1.

- When using a scanl, the final result will be in the last element of the resulting list while a scanr will place the result in the head.

```hs
scanl (+) 0 [3,5,2,1] --[0,3,8,10,11]
scanr (+) 0 [3,5,2,1] --[11,8,3,1,0]
scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1] --[3,4,5,5,7,9,9,9]
scanl (flip (:)) [] [3,2,1] --[[],[3],[2,3],[1,2,3]]
```

Exercises:

1. How many elements does it take for the sum of the roots of all natural numbers to exceed 1000?
```hs
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
```

--- 

<a name="pesos"></a>
#### Function application with $

```hs
($) :: (a -> b) -> a -> b
f $ x = f x
```

Function application with $ is **right-associative** and has the **lowest precedence**.

Most of the time, it's a convenience function so that we don't have to write so many *parentheses*.

When a $ is encountered, the expression on its right is applied as the parameter to the function on its left. 

$ means that **function application can be treated just like another function**. That way, we can, for instance, map function application over a list of functions.

```hs
map ($ 3) [(4+), (10*), (^2), sqrt] --[7.0,30.0,9.0,1.7320508075688772]

sqrt $ 3 + 4 + 9
sum $ filter (> 10) $ map (*2) [2..10]
```

--- 

<a name="Function_composition"></a>
#### Function composition

Many times, a point free style is more readable and concise

```hs
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)
```

```hs
map (negate . abs) [5,-3,-6,7,-3,2,-19,24] --[-5,-3,-6,-7,-3,-2,-19,-24]
```

what about functions that take several parameters? 
If we want to use them in function composition, we usually have to partially apply them just so much that each function takes just one parameter

```hs
replicate 5 . max 6.7 $ 8.9 --[8.9,8.9,8.9,8.9,8.9]
```

Exercise:

1. Find the sum of all odd squares that are smaller than 10,000. 
```hs
oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

oddSquareSum :: Integer
oddSquareSum = 
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<10000) oddSquares
    in  sum belowLimit
```

---

<a name="modulos"></a>
## Modules

### Data.Char

```hs
digitToInt :: Char -> Int
```

### Data.List

```hs
-- Elimina duplicados de una lista
nub :: Eq a => [a] -> [a]

-- Toma una lista y agrupa los elementos adyacentes que sean iguales en sublistas.
group :: Eq a => [a] -> [[a]]

sort :: Ord a => [a] -> [a]

-- Ordene una lista comparando los resultados de una función clave aplicada a cada elemento
-- sortOn f es equivalente a sortBy (comparando f),
-- 	pero tiene la ventaja de rendimiento de evaluar f solo una vez para cada elemento en la lista de entrada.
-- 	Esto se denomina paradigma de decorar-ordenar-desdecorar o transformada de Schwartz.
sortOn :: Ord b => (a -> b) -> [a] -> [a]

```

```hs
lookup :: Eq a => a -> [(a, b)] -> Maybe b

-- Mi implementación
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing
```

### Data.Map

Las **listas de asociación** (también llamadas **diccionarios**) son listas que son utilizadas para almacenar pares *clave-valor* donde el orden no importa.

```hs
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing
```

```hs
import Data.Map

-- Tomo una lista de asociación (en forma de lista) y devuelve un diccionario con las mismas asociaciones.
-- En caso de que existan claves duplicadas en la lista de asociación, los duplicados son descartados.
Data.Map.fromList :: Ord k => [(k, a)] -> Map k a
```

### Data.Set

Remueve duplicados & Composición de funciones & Conjuntos
```hs
import Data.Set

quitar_duplicados :: (Ord a) => [a] -> [a]
quitar_duplicados = toList . fromList
```

### Data.Tree
