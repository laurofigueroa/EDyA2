module Lab01 where

import Data.List
import Data.Char

{-
1) Corregir los siguientes programas de modo que sean aceptados por GHCi.
-}

-- a)
notl :: Bool -> Bool
notl b = case b of
    True -> False
    False -> True

-- b)
in2 :: [a] -> [a]
in2 [x]         =  []
in2 (x:xs)      =  x : in2 xs
in2 []          =  error "empty list"

-- c)
length2 []        =  0
length2 (_:l)     =  1 + length2 l

-- d)
list123 :: [Int]
list123 = 1 : 2 : 3 : []

-- e)
-- ++! :: [a] -> [a]
[]     ++! ys = ys
(x:xs) ++! ys = x : (xs ++! ys)

-- f)
addToTail :: Num a => a -> [a] -> [a]
addToTail x xs = map (+x) (tail xs)


-- g)
listmin :: Ord a => [a] -> a
listmin xs = head (sort xs)

-- h) (*)
smap :: (a -> b) -> [a] -> [b]
smap f [] = []
smap f [x] = [f x]
smap f (x:xs) = f x : smap f xs

--2. Definir las siguientes funciones y determinar su tipo:

--a) five, que dado cualquier valor, devuelve 5

five :: Num a => a -> Int
five x = 5

--b) apply, que toma una función y un valor, y devuelve el resultado de
--aplicar la función al valor dado

apply :: (a -> b) -> a -> b
apply f x = f x

--c) ident, la función identidad

ident :: a -> a
ident x = x

--d) first, que toma un par ordenado, y devuelve su primera componente

first :: (a,b) -> a
first (x,y) = x

--e) derive, que aproxima la derivada de una función dada en un punto dado

derive :: Fractional a => (a -> a) -> a -> a
derive f x = (f (x+0.01) - f x) / (0.01)

--f) sign, la función signo

sign :: (Ord a, Num a) => a -> Char
sign x = if x >= 0 then '+' else '-'

sign2 :: (Ord a, Num a) => a -> a
sign2  x | x > 0 = 1
         | x ==0 = 0
         | x < 0 = -1

--g) vabs, la función valor absoluto (usando sign y sin usarla)

-- Version con sign

vabs :: (Ord a, Num a) => a -> a
vabs x = if (sign(x) == '-') then (-1)*x else x

vabsM :: (Ord a, Num a) => a -> a
vabsM x =  x * sign2 x

-- Version sin sign
vabs2 :: (Ord a, Num a) => a -> a
vabs2 x = if x < 0 then (-1)*x else x

--h) pot, que toma un entero y un número, y devuelve el resultado de
--elevar el segundo a la potencia dada por el primero

pot :: Integer -> Integer -> Integer
pot 0 x = 1
pot 1 x = x
pot y x = x * pot (y-1) x

--i) xor, el operador de disyunción exclusiva

--xor :: [Bool] -> Bool

--j) max3, que toma tres números enteros y devuelve el máximo entre llos

max2 :: Integer -> Integer -> Integer
max2 x y = if x < y then y else x

max3 :: Integer -> Integer -> Integer -> Integer
max3 x y z = max (max x y) (max x z)

--k) swap, que toma un par y devuelve el par con sus componentes invertidas

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

{- 
3) Definir una función que determine si un año es bisiesto o no, de
acuerdo a la siguiente definición:

año bisiesto 1. m. El que tiene un día más que el año común, añadido al mes de febrero. Se repite
cada cuatro años, a excepción del último de cada siglo cuyo número de centenas no sea múltiplo
de cuatro. (Diccionario de la Real Academia Espaola, 22ª ed.)

¿Cuál es el tipo de la función definida?
-}

bisiesto::Int->Bool
bisiesto a = mod a 4==0 && mod a 100/=0 || mod a 400 ==0

añobisiesto :: Int -> Bool
añobisiesto x = if mod x 4 == 0 && notl ((mod x 100 == 0) && notl(mod x 400 == 0)) then True else False

-- test para comprobar añobisiesto
test :: Int -> Bool
test 4 = True
test x =  (bisiesto (x) == añobisiesto (x))  && test (x-1)

{-
4)

Defina un operador infijo *$ que implemente la multiplicación de un
vector por un escalar. Representaremos a los vectores mediante listas
de Haskell. Así, dada una lista ns y un número n, el valor ns *$ n
debe ser igual a la lista ns con todos sus elementos multiplicados por
n. Por ejemplo,

[ 2, 3 ] *$ 5 == [ 10 , 15 ].

El operador *$ debe definirse de manera que la siguiente
expresión sea válida:


v = [1, 2, 3] *$ 2 *$ 4

-}

(*$) :: [Integer] -> Integer -> [Integer]
[] *$ k = []
(x:xs) *$ k =  k*x : (xs *$ k)

{-
5) Definir las siguientes funciones usando listas por comprensión:
-}

{-
a) 'divisors', que dado un entero positivo 'x' devuelve la
lista de los divisores de 'x' (o la lista vacía si el entero no es positivo)
-}

divisors :: Integer -> [Integer]
divisors x = [y | y <- [1..x], mod x y == 0]

{-
b) 'matches', que dados un entero 'x' y una lista de enteros descarta
de la lista los elementos distintos a 'x'
-}

matches :: Integer -> [Integer] -> [Integer]
matches x ys = [k | k <- ys, x == k]

{-
c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas
'(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2,
donde 0 <= a, b, c, d <= 'n'
-}

cuadrupla :: Integer -> [(Integer, Integer, Integer, Integer)]
cuadrupla n = [(a,b,c,d) | a <- [0..n], b <- [0..n], c <- [0..n], d <- [0..n], pot 2 a + pot 2 b == pot 2 c + pot 2 d]

{-
(d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista
'xs' sin elementos repetidos
-}

unique :: [Integer] -> [Integer]
unique xs = [x | (x,i) <- zip xs [0..], not(elem x(take i xs))] 

{- 
6) El producto escalar de dos listas de enteros de igual longitud
es la suma de los productos de los elementos sucesivos (misma
posición) de ambas listas.  Definir una función 'scalarProduct' que
devuelva el producto escalar de dos listas.

Sugerencia: Usar las funciones 'zip' y 'sum'.
-}

scalarProduct :: [Integer] -> [Integer] -> Integer
scalarProduct xs ys = sum [x*y | (x,y) <- zip xs ys]


{-
7) Definir mediante recursión explícita
las siguientes funciones y escribir su tipo más general:

a) 'suma', que suma todos los elementos de una lista de números
-}
suma :: Num a => [a] -> a
suma [] = 0
suma (x:xs) = x + suma xs

{-
b) 'alguno', que devuelve True si algún elemento de una
lista de valores booleanos es True, y False en caso
contrario
-}

alguno :: [Bool] -> Bool
alguno [] = False
--alguno [True] = True
--alguno [False] = False
alguno (x:xs) = x || alguno xs

{-
c) 'todos', que devuelve True si todos los elementos de
una lista de valores booleanos son True, y False en caso
contrario
-}

todos :: [Bool] -> Bool
todos [] = True
todos (x:xs) = x && todos xs

{-
d) 'codes', que dada una lista de caracteres, devuelve la
lista de sus ordinales
-}

codes :: [Char] -> [Int]
codes [] = []
codes (x:xs) =  ord x : codes xs

{-
e) 'restos', que calcula la lista de los restos de la
división de los elementos de una lista de números dada por otro
número dado
-}

restos :: [Integer] -> Integer -> [Integer]
restos [] _ = []
restos (x:xs) y = (mod x y) : restos xs y


{-
f) 'cuadrados', que dada una lista de números, devuelva la
lista de sus cuadrados
-}

cuadrados :: [Integer] -> [Integer]
cuadrados [] = []
cuadrados (x:xs) = (x*x) : cuadrados xs

{-
g) 'longitudes', que dada una lista de listas, devuelve la
lista de sus longitudes
-}

longitudes :: [[a]] -> [Int]
longitudes [] = []
longitudes (x:xs) = (length x) : longitudes xs

{-
h) 'orden', que dada una lista de pares de números, devuelve
la lista de aquellos pares en los que la primera componente es
menor que el triple de la segunda
-}

orden :: [(Integer, Integer)] -> [(Integer, Integer)]
orden [] = []
orden ((x,y):xs) = if x < 3*y then (x,y):orden xs else orden xs

{-
i) 'pares', que dada una lista de enteros, devuelve la lista
de los elementos pares
-}

pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs) = if mod x 2 == 0 then x : pares xs else pares xs

{-
j) 'letras', que dada una lista de caracteres, devuelve la
lista de aquellos que son letras (minúsculas o mayúsculas)
-}

letras :: [Char] -> [Char]
letras [] = []
letras (x:xs)  | 'a'<= x && x <= 'z' = x : letras xs
               | 'A'<= x && x <= 'Z' = x : letras xs
               | otherwise           = letras xs

{-
k) 'masDe', que dada una lista de listas 'xss' y un
número 'n', devuelve la lista de aquellas listas de 'xss'
con longitud mayor que 'n' 
-}

masDe :: [[a]] -> Int -> [[a]]
masDe [] _ = []
masDe (x:xss) n = if length x <= n then masDe xss n else x : masDe xss n

