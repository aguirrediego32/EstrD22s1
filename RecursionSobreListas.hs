--1.Dada una lista de enteros devuelve la suma de todos sus elementos
sumatoria :: [Int] -> Int
sumatoria []        = 0
sumatoria (x:xs)    = x + sumatoria xs

--2. Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad
--de elementos que posee
longitud :: [a] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs    

--3. Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
sucesores :: [Int] -> [Int]
sucesores []        = []
sucesores (x:xs)    = x + 1 : sucesores xs

--4. Dada una lista de booleanos devuelve True si todos sus elementos son True
disyuncion :: [Bool] -> Bool
disyuncion []       = True
disyuncion (x:xs)   = x && disyuncion xs

--5. Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
conjuncion :: [Bool] -> Bool
conjuncion []       = False
conjuncion (x:xs)   = x || conjuncion xs 

--6. Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar :: [[a]] -> [a]
aplanar []          = []
aplanar (xs:xss)    = xs ++  aplanar xss

--7. Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual a e.
pertenece :: Eq a => a -> [a] -> Bool
pertenece e []      = False
pertenece e (x:xs)  = e == x ||  pertenece e xs

--8. Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
apariciones :: Eq a => a -> [a] -> Int
apariciones e []        = 0
apariciones e (x:xs)    = if e == x 
                            then 1 + apariciones e xs
                            else apariciones e xs    

--9 Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n []        = []
losMenoresA n (x:xs)    = if n > x
                            then x : losMenoresA n xs
                            else losMenoresA n xs    

--10. Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más de n elementos.
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n []        = []
lasDeLongitudMayorA n (xs:xss)  = if n < (longitud xs)
                                    then xs : lasDeLongitudMayorA n xss
                                    else lasDeLongitudMayorA n xss

--11. Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la lista.
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e     = [e]
agregarAlFinal xs e     =  xs ++ [e]

--12. Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los
--elementos de la segunda a continuación. Definida en Haskell como ++
concatenar :: [a] -> [a] -> [a]
concatenar [] ys        = ys
concatenar xs []        = xs
concatenar (x:xs) ys    = x : concatenar xs ys

--13. Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida
--en Haskell como reverse
reversa :: [a] -> [a]
reversa []      = []
reversa (x:xs)  = reversa xs ++ [x]

{-14. Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
las listas no necesariamente tienen la misma longitud.-}
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] ys            = ys
zipMaximos xs []            = xs
zipMaximos (x:xs) (y:ys)    = if x > y 
                                then x : zipMaximos xs ys
                                else y : zipMaximos xs ys   

--15. Dada una lista devuelve el mínimo
elMinimo :: Ord a => [a] -> a
elMinimo []         = error " tiene que tener al menos 1 elemento"
elMinimo (x : [])   = x
elMinimo (x:xs)     = min x (elMinimo xs) 

--Recursión sobre números
--1. Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta
--llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)    

--2. Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre
--n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n - 1)    

--3. Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces
repetir :: Int -> a -> [a]
repetir 0 e = []
repetir n e =  e : repetir (n - 1) e 

--4. Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs.
--Si la lista es vacía, devuelve una lista vacía.
losPrimeros :: Int -> [a] -> [a]
losPrimeros _ []        = []
losPrimeros n (x:xs)    = if (n > 0)
                        then x : losPrimeros (n - 1 ) xs
                        else [] 

--5. Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista
--recibida. Si n es cero, devuelve la lista completa.
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros x []     = []
sinLosPrimeros 0 xs     = xs
sinLosPrimeros n (x:xs) = sinLosPrimeros (n- 1) xs

--Registros
type Nombre     = String
type Edad       = Int
data Persona    = P Nombre Edad  deriving Show
yo = P "Diego" 35
jorge = P "Jorge" 17
maria = P "Maria" 20
lista1 = [yo,jorge,maria]

--Dados una edad y una lista de personas devuelve a las personas mayores a esa edad.
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n []       = [] 
mayoresA  n (p:ps)    = if (esMayor n p)
                        then p : mayoresA n ps
                        else mayoresA n ps

--Dada un numero y una Persona da True si la edad de la Persona es mayor al numero dado sino da False
esMayor ::Int -> Persona -> Bool
esMayor n (P _ e) = e > n 

--Dada una lista de personas devuelve el promedio de edad entre esas personas. Precondición: la lista al menos posee una persona
promedioEdad :: [Persona] -> Int
promedioEdad [] = error "tiene que existir al menos 1 persona"
promedioEdad ps = div (edadTotal ps) (longitud ps) 

--Dada una Persona determino su edad
edad :: Persona -> Edad
edad (P _ e) = e

--Dada una lista de Persona devuelvo la suma de todas al edades
edadTotal :: [Persona] -> Int
edadTotal [] = 0
edadTotal (p:ps) = edad p + edadTotal ps    

--Dada una lista de personas devuelve la persona más vieja de la lista. Precondición: la
--lista al menos posee una persona.
elMasViejo :: [Persona] -> Persona
elMasViejo (p : []) = p
elMasViejo (p:ps) =  elMayor p (elMasViejo ps)

--Dada dos personas devuelve la que es mayor
elMayor :: Persona -> Persona -> Persona
elMayor p1 p2  = if esMayorQueLaOtra p1 p2
                         then p1 
                         else p2

--Dada dos Personas devuelve True si la primera es mayor
esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = (edad p1) > (edad p2)

data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]
  
