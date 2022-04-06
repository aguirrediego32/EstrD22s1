--1. Defina las siguientes funciones:
--a) Dado un número devuelve su sucesor
sucesor :: Int -> Int
sucesor x = x + 1

--b) Dados dos números devuelve su suma utilizando la operación +.
sumar :: Int -> Int -> Int
sumar n m = n + m

{-c)Dado dos números, devuelve un par donde la primera componente es la división del
primero por el segundo, y la segunda componente es el resto de dicha división. Nota:
para obtener el resto de la división utilizar la función mod :: Int -> Int -> Int,
provista por Haskell.-}
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto m n = (div m n, mod m n)

--d)Dado un par de números devuelve el mayor de estos
maxDelPar :: (Int,Int) -> Int
maxDelPar (m,n) = if (m > n)
                    then m
                    else n
{-2. De 4 ejemplos de expresiones diferentes que denoten el número 10, utilizando en cada expresión a todas las funciones del punto anterior.
Ejemplo: maxDePar (divisionYResto (suma 5 5) (sucesor 0))-}
--sumar(maxDelPar(divisionYResto (sucesor 17) 2)) 1
--sucesor (sumar( maxDelPar(divisionYResto 20 5))5)
--maxDelPar(divisionYResto (sumar 90  (sucesor 6)) 9)
--sumar(maxDelPar(divisionYResto 2 10)) (sucesor 7)

--TIPOS ENUMERATIVOS
data Dir = Norte |Sur |Este |Oeste 

--a) Dada una dirección devuelve su opuesta
opuesto :: Dir -> Dir
opuesto Norte   = Sur
opuesto Sur     = Norte
opuesto Este    = Oeste
opuesto Oeste   = Este

--b) Dadas dos direcciones, indica si son la misma. Nota: utilizar pattern matching y no ==.
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur     = True
iguales Este Este   = True
iguales Oeste Oeste = True
iguales _ _         = False

{-c) Dada una dirección devuelve su siguiente, en sentido horario, y suponiendo que no existe
la siguiente dirección a Oeste. ¿Posee una precondición esta función? ¿Es una función
total o parcial? ¿Por qué?-}
--Precondición: No se puede obtener siguiente dirección de Oeste
--Es una función parcial porque no cubre todos los casos
siguiente :: Dir -> Dir
siguiente Norte     = Este
siguiente Este      = Sur
siguiente Sur       = Oeste
siguiente Oeste     = error "No hay siguiente direccion"

data DiaDeSemana = Lunes|Martes|Miercoles|Jueves|Viernes|Sabado|Domingo 

{-a) Devuelve un par donde la primera componente es el primer día de la semana, y la
segunda componente es el último día de la semana.-}
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes,Domingo)

--b) Dado un dia de la semana indica si comienza con la letra M.
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes      = True
empiezaConM Miercoles   = True
empiezaConM _           = False

--c) Dado dos dias de semana, indica si el primero viene después que el segundo
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues d1 d2 = nroDia d1 > nroDia d2

--Dado un dia de semana asigna el numero correspondiente en la semana
nroDia :: DiaDeSemana -> Int
nroDia Lunes        = 1
nroDia Martes       = 2
nroDia Miercoles    = 3
nroDia Jueves       = 4
nroDia Viernes      = 5
nroDia Sabado       = 6
nroDia Domingo      = 7

--d) Dado un dia de la semana indica si no es ni el primer ni el ultimo dia
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes     = False
estaEnElMedio Domingo   = False
estaEnElMedio _         = True

{-a) Dado un booleano, si es True devuelve False, y si es False devuelve True.
En Haskell ya está definida como not.-}
negar :: Bool -> Bool
negar True = False
negar False = True

{-b) Dados dos booleanos, si el primero es True y el segundo es False, devuelve False, sino
devuelve True-}
implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True

{-c) Dados dos booleanos si ambos son True devuelve True, sino devuelve False.
En Haskell ya está definida como \&\&-}
and :: Bool -> Bool -> Bool
and True True = True 
and _ _ = False 

{-d) Dados dos booleanos si alguno de ellos es True devuelve True, sino devuelve False.
En Haskell ya está definida como ||.-}
or :: Bool -> Bool -> Bool
or False False = False
or _ _ = True




-- REGISTROS

type Nombre = String
type Edad = Int 
data Persona = P Nombre Edad 

yo = P "Diego" 35
el = P "Jorge" 30

--Devuelve el nombre de una persona
nombre :: Persona -> String
nombre (P n e) = n

--Devuelve la edad de una persona
edad :: Persona -> Int
edad (P n e) = e

--Aumenta en uno la edad de la persona
crecer :: Persona -> Persona
crecer (P n e) = P n (e +1)

{-Dados un nombre y una persona, devuelve una persona con la edad de la persona y el
nuevo nombre.-}
cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nn (P n e ) = P nn e

--Dadas dos personas indica si la primera es mayor que la segunda
esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = edad p1 > edad p2

--Dadas dos personas devuelve a la persona que sea mayor.
laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if (esMayorQueLaOtra p1 p2)
                        then p1
                        else p2
--Pokemon
data TipoDePokemon = Fuego |Agua |Planta   
type Energia = Int
data Pokemon = Poke TipoDePokemon Energia   
data Entrenador = E Nombre Pokemon Pokemon 

bulbasaur = Poke Planta 20
squirtle = Poke Agua 19
vulpix = Poke Fuego 15
blastoise = Poke Agua 25

ash = E "Ash" bulbasaur bulbasaur
brock = E "Brock" blastoise vulpix

{-Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua
supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.-}
superaA :: Pokemon -> Pokemon -> Bool
superaA p1 p2 = leGana (obtenerTipo p1) (obtenerTipo p2)

--Dado dos tipo de pokemon determina si el primero vence al segundo
leGana :: TipoDePokemon -> TipoDePokemon -> Bool
leGana Fuego Planta = True
leGana Planta Agua = True
leGana Agua Fuego = True
leGana _ _ = False 

-- Dado un pokemon denota el tipo
obtenerTipo :: Pokemon -> TipoDePokemon
obtenerTipo (Poke t e) = t

--Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe t (E n p1 p2) = (unoOCero t p1) + (unoOCero t p2)

--Dado un TipoDePokemon y 1 Pokemon determina si es del mismo Tipo de Pokemon

esDelMismoTipo :: TipoDePokemon -> Pokemon -> Bool
esDelMismoTipo t p = sonIguales t (obtenerTipo p)

-- Dado 1 TipoDePokemon y 1 Pokemon  devuelve 0 o 1 si son iguales
unoOCero :: TipoDePokemon ->Pokemon -> Int
unoOCero t p = if (esDelMismoTipo t p) then 1 else 0

--Dado 2 TipoDePokemon determna si son iguales
sonIguales :: TipoDePokemon -> TipoDePokemon -> Bool
sonIguales Planta Planta    = True
sonIguales Fuego Fuego      = True
sonIguales Agua Agua        = True
sonIguales _ _              = False

--Dado un par de entrenadores, devuelve a sus Pokémon en una lista.
juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (e1,e2) = pokemones e1 ++ pokemones e2

pokemones :: Entrenador -> [Pokemon]
pokemones (E n p1 p2) = [p1, p2]

--Funciones polimórficas
--a)Dado un elemento de algún tipo devuelve ese mismo elemento.
loMismo :: a -> a
loMismo x = x

--b) Dado un elemento de algún tipo devuelve el número 7.
siempreSiete :: a -> Int
siempreSiete x = 7

--c) Dadas una tupla, invierte sus componentes.¿Por qué existen dos variables de tipo diferentes?
--Existen 2 variables de tipo diferente para determinar que pueden ser de distinto tipo
--Son polimórficas porque no importa los datos específicos solo la estructura
swap :: (a,b) -> (b, a)
swap (x,y) = (y,x)

-- Pattern matching sobre listas
--2. Dada una lista de elementos, si es vacía devuelve True, sino devuelve False.
--Definida en Haskell como null.
estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False 

{-3. Dada una lista devuelve su primer elemento.
Definida en Haskell como head.
Nota: tener en cuenta que el constructor de listas es :-}
elPrimero :: [a] -> a
elPrimero [] = error "No puedo devolver el primero en lista vacia"
elPrimero(x:_) = x

{-4. Dada una lista devuelve esa lista menos el primer elemento.
Definida en Haskell como tail.
Nota: tener en cuenta que el constructor de listas es :-}
sinElPrimero :: [a] -> [a]
sinElPrimero [] = error "tiene que haber 1 elemento como minimo"
sinElPrimero (_:xs) = xs

{-5. Dada una lista devuelve un par, donde la primera componente es el primer elemento de la
lista, y la segunda componente es esa lista pero sin el primero.
Nota: tener en cuenta que el constructor de listas es :-}
splitHead :: [a] -> (a, [a])
splitHead [] = error "tiene que haber 1 elemento minimo"
splitHead [x] = (x, (sinElPrimero [x]))
splitHead (x:xs) = (x, (sinElPrimero (x:xs)))

