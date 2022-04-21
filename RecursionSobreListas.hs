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
agregarAlFinal [] e     = e : [] 
agregarAlFinal (x:xs) e    = x : agregarAlFinal xs e 

--12. Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los
--elementos de la segunda a continuación. Definida en Haskell como ++
concatenar :: [a] -> [a] -> [a]
concatenar [] ys        = ys
concatenar (x:xs) ys    = x : concatenar xs ys

--13. Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida
--en Haskell como reverse
reversa :: [a] -> [a]
reversa []      = []
reversa (x:xs)  = agregarAlFinal (reversa xs) x 

{-14. Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
las listas no necesariamente tienen la misma longitud.-}
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos xs []            = xs
zipMaximos [] ys            = ys
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
cuentaRegresiva n = if (n < 1)
                        then []
                        else n : cuentaRegresiva (n - 1)    

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
data Persona    = P Nombre Edad  
yo = P "Diego" 35
jorge   = P "Jorge" 17
maria   = P "Maria" 20
lista1  = [yo,jorge,maria]

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
edadTotal []        = 0
edadTotal (p:ps)    = edad p + edadTotal ps    

--Dada una lista de personas devuelve la persona más vieja de la lista. Precondición: la
--lista al menos posee una persona.
elMasViejo :: [Persona] -> Persona
elMasViejo []       = error "No existe la persona mas vieja en lista vacia"
elMasViejo (p : []) = p
elMasViejo (p:ps)   =  elMayor p (elMasViejo ps)

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

bulbasaur = ConsPokemon Planta 20
squirtle = ConsPokemon Agua 19
vulpix = ConsPokemon Fuego 15

ash = ConsEntrenador "Ash" [bulbasaur,squirtle, squirtle, vulpix, vulpix,vulpix]
brock = ConsEntrenador "Brock" [vulpix, vulpix]

--Devuelve la cantidad de Pokémon que posee el entrenador. 
cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador _ pks) = longitud pks

-- Dado un pokemon denota el tipo
obtenerTipo :: Pokemon -> TipoDePokemon
obtenerTipo (ConsPokemon t e) = t
--Dado un TipoDePokemon y 1 Pokemon determina si es del mismo Tipo de Pokemon
esDelMismoTipo :: TipoDePokemon -> Pokemon -> Bool
esDelMismoTipo t p = sonIguales t (obtenerTipo p)


--Dado 2 TipoDePokemon determna si son iguales
sonIguales :: TipoDePokemon -> TipoDePokemon -> Bool
sonIguales Planta Planta    = True
sonIguales Fuego Fuego      = True
sonIguales Agua Agua        = True
sonIguales _ _              = False

--Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t (ConsEntrenador _ pks) = cantTotal t pks

cantTotal:: TipoDePokemon -> [Pokemon]-> Int
cantTotal t [] = 0
cantTotal t (pk:pks) = if esDelMismoTipo t pk     
                            then 1 + cantTotal t pks
                            else cantTotal t pks


--Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo, que le ganarían
--a los Pokemon del segundo entrenador.
losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
losQueLeGanan t  (ConsEntrenador _ pk1)(ConsEntrenador _ pk2) = totalGanador t pk1 pk2

totalGanador:: TipoDePokemon -> [Pokemon] -> [Pokemon] -> Int
totalGanador t [] ps2 = 0
totalGanador t (p:ps) ps2 = if esDelMismoTipo t p && leGanaATodos p ps2
                            then 1 + totalGanador t ps ps2
                            else totalGanador t ps ps2


--Dado un TipoDePokemon y una lista de Pokemon determina si el tipo dado vence a todos los pokemon de la lista
-- en función de los tipos
leGanaATodos:: Pokemon ->[Pokemon] ->Bool
leGanaATodos p [] = True
leGanaATodos p (pk:pks) = superaA p pk  && (leGanaATodos p pks)     



--Dado dos tipo de pokemon determina si el primero vence al segundo
leGana :: TipoDePokemon -> TipoDePokemon -> Bool
leGana Fuego Planta = True
leGana Planta Agua = True
leGana Agua Fuego = True
leGana _ _ = False 

superaA :: Pokemon -> Pokemon -> Bool
superaA p1 p2 = leGana (obtenerTipo p1) (obtenerTipo p2)

--Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador _ pks) = todosLosTipos pks

todosLosTipos :: [Pokemon] -> Bool
todosLosTipos pk = perteneceTipo Planta pk && perteneceTipo Fuego pk && perteneceTipo Agua pk

perteneceTipo :: TipoDePokemon -> [Pokemon] -> Bool
perteneceTipo t1 []      = False
perteneceTipo t1 (t:ts)  = sonIguales t1 (obtenerTipo t) ||  perteneceTipo t1 ts

--Proyectos

data Seniority = Junior | SemiSenior | Senior                                  
data Proyecto = ConsProyecto String                                            
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto        
data Empresa = ConsEmpresa [Rol]                                             


proyecto1 = ConsProyecto "Tecnologia"
proyecto2 = ConsProyecto "Economia"
proyecto3 = ConsProyecto "Banco Galicia"
proyecto4 = ConsProyecto "Escuela"

listaProyectos = [proyecto2, proyecto2, proyecto2, proyecto4, proyecto3,proyecto4]
listaProyectos2 = [proyecto2, proyecto2, proyecto2, proyecto4, proyecto3,proyecto1]
listaRol = [(Developer Junior proyecto1),(Management Junior proyecto2),(Management Junior proyecto1),(Developer Senior proyecto1)]

listaRol2 = [(Developer Senior proyecto2),(Management Junior proyecto2),(Management Junior proyecto1),(Developer SemiSenior proyecto3)]

listaRol3 = [(Developer Senior proyecto2), (Management Junior proyecto2), (Developer Senior proyecto1),(Management Junior proyecto1),(Developer SemiSenior proyecto3),(Developer Senior proyecto4)]
rolSeniorM = (Management Senior proyecto2)
rolSeniorD = (Developer Senior proyecto1)
rolJuniorD = (Developer Junior proyecto1)
rolJuniorM = (Management Junior proyecto1)

empresa2 = ConsEmpresa [Management Senior proyecto3,Developer Senior proyecto3,Developer Senior proyecto2, Developer Senior proyecto1, Developer SemiSenior proyecto1]

empresa1 = ConsEmpresa [Developer Senior proyecto1, Management Senior proyecto1, Developer SemiSenior proyecto3,Developer Junior proyecto3,Management Senior proyecto3, Management Senior proyecto1]

--Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.
proyectos :: Empresa -> [Proyecto]
proyectos empresa =  proyectosUnicos(listaDeProyectos(rolesEmpresa(empresa)))

--Dado una empresa devuelve la lista de Rol que tiene
rolesEmpresa :: Empresa -> [Rol]
rolesEmpresa (ConsEmpresa roles) = roles

--Dado un Rol devuelve el proyecto que tiene
proyecto :: Rol -> Proyecto
proyecto (Developer _ pr) = pr
proyecto (Management _ pr) = pr

--Dado un proyecto devuelve su nombre
nombreProyecto :: Proyecto -> String
nombreProyecto (ConsProyecto n) = n

--Dado una lista de Rol devuelve una lista con todos los proyectos 
listaDeProyectos :: [Rol] -> [Proyecto]
listaDeProyectos [] = []
listaDeProyectos (r:rs) = proyecto r : listaDeProyectos rs

nombresDeProyectos :: [Proyecto] -> [String]
nombresDeProyectos [] = []
nombresDeProyectos (p:ps) =  nombreProyecto p : nombresDeProyectos ps

--Dada una lista de proyectos devuelve una lista de proyectos sin repetidos
proyectosUnicos :: [Proyecto] -> [Proyecto]
--proyectosUnicos []      = []
proyectosUnicos (p:ps)  = incluirSiNoEsta p (proyectosUnicos ps) 


incluirSiNoEsta:: Proyecto -> [Proyecto] -> [Proyecto]
incluirSiNoEsta x ys = if pertenece (nombreProyecto x) (nombresDeProyectos ys)
                then ys
                else x: ys

--Dado un Seniority devuelve True si es Senior false en caso contrario
esSenior :: Seniority -> Bool
esSenior Senior = True
esSenior _ = False

--Dado un rol determina si es desarrollador y si es senior
esDesarrolladorSenior :: Rol -> Bool
esDesarrolladorSenior (Developer senrt _) = esSenior senrt
esDesarrolladorSenior (Management senrt _) = False


--Dado una lista de Rol devuelve una lista de Rol solo con los Desarrolladores Senior
soloDevSenior :: [Rol] -> [Rol]
soloDevSenior [] = []
soloDevSenior (r:rs) = if esDesarrolladorSenior r
                        then r : soloDevSenior rs
                        else soloDevSenior rs

-- Dado una lista de nombres y una lista de Rol filtra los roles que pertenecen a la lista de nombres,
--es decir si el desarrollador trabaja en la lista de nombres ( los roles pasados por parametro ya contienen desarrolladores unicamente)
empleadosQuePertenecen :: [String] -> [Rol] -> [Rol]
empleadosQuePertenecen nombres [] = []
empleadosQuePertenecen nombres (r:rs) = if pertenece (nombreProyecto(proyecto r)) nombres
                                                then r : empleadosQuePertenecen nombres rs
                                                else empleadosQuePertenecen nombres rs

--Dado una lista de Proyecto y una lista de Rol devuelve una lista de Rol con los desarrolladores que estan incluidos en la lista de proyectos
desarrolladoresIncluidosEnProyecto :: [Proyecto] -> [Rol] -> [Rol]
desarrolladoresIncluidosEnProyecto ps  rol = empleadosQuePertenecen (nombresDeProyectos ps) rol  
  
--Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertenecen
--además a los proyectos dados por parámetro
losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior emp proyectos = longitud (desarrolladoresIncluidosEnProyecto proyectos (soloDevSenior(rolesEmpresa emp)))

--Indica la cantidad de empleados que trabajan en alguno de los proyectos dados
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn pr emp    = longitud (desarrolladoresIncluidosEnProyecto pr (rolesEmpresa emp)) 

--Dada un rol(empleado) y un proyecto devuelve True si esta en el mismo proyecto
trabajaEnElProyecto :: Proyecto -> Rol -> Bool
trabajaEnElProyecto unPr (Developer _ pr)   = (nombreProyecto unPr) == (nombreProyecto pr)
trabajaEnElProyecto unPr (Management _ pr)  = (nombreProyecto unPr) == (nombreProyecto pr)


--Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su cantidad de personas involucradas.
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (ConsEmpresa rs) = asignadosPorProyectoAux rs 


asignadosPorProyectoAux :: [Rol] -> [(Proyecto, Int)]
asignadosPorProyectoAux []     = []
asignadosPorProyectoAux (r:rs) = if perteneceProyecto (proyecto r)   (asignadosPorProyectoAux rs )
                                   then sumarUnoAProyecto (proyecto r) (asignadosPorProyectoAux rs) 
                                   else (proyecto r, 1) : asignadosPorProyectoAux rs   

-- Dado un Proyecto y una lista de pares Proyecto Int suma 1 al par del proyecto dado
--Precondicion: la lista no esta vacia, esta función se usa dentro de perteneceProyecto.
sumarUnoAProyecto :: Proyecto -> [(Proyecto, Int)] -> [(Proyecto, Int)]
sumarUnoAProyecto p (x:xs) = if esMismoProyecto p (fst x)
                               then (p, snd x + 1) : xs
                               else x : sumarUnoAProyecto p xs


perteneceProyecto :: Proyecto -> [(Proyecto, Int)] -> Bool
perteneceProyecto p []     = False
perteneceProyecto p (x:xs) = esMismoProyecto p  (fst x)  || perteneceProyecto p xs

esMismoProyecto:: Proyecto -> Proyecto -> Bool
esMismoProyecto p1 p2 = nombreProyecto p1 == nombreProyecto p2














