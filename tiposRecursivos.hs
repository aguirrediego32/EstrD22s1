data Color = Azul | Rojo                        deriving Show
data Celda = Bolita Color Celda | CeldaVacia    deriving Show

--Celda ejemplo
celda2Azul2Rojo = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))


--Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya
--existe una operación sobre listas que ayude a resolver el problema.
nroBolitas:: Color -> Celda -> Int
nroBolitas c CeldaVacia         = 0
nroBolitas unCol (Bolita col cel)   = unoSi(esMismoColor unCol col) + nroBolitas unCol cel

--Dado dos colores devuelve true si son del mismo color
esMismoColor:: Color -> Color -> Bool
esMismoColor Azul Azul  = True
esMismoColor Rojo Rojo  = True
esMismoColor _ _        = False

unoSi:: Bool -> Int
unoSi True  = 1
unoSi False = 0

--Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner :: Color -> Celda -> Celda
poner unCol CeldaVacia          = Bolita unCol CeldaVacia
poner unCol (Bolita col cel)    = Bolita unCol (poner col cel)    


--Dado un color y una celda saca una bolita del color dado, si no hubiese nada en la celda queda una celda vacia
sacar :: Color -> Celda -> Celda
sacar unColor CeldaVacia = CeldaVacia
sacar unColor (Bolita color unaCelda) = if  (esMismoColor unColor color)
                                         then unaCelda 
                                         else  Bolita  color (sacar unColor unaCelda)


--Dado una cantidad un color y una celda agrega bolitas del color y la cantidad dada a la celda
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 unColor celda      = celda
ponerN n unColor unaCelda   = poner unColor (ponerN (n - 1) unColor unaCelda)

data Objeto = Cacharro | Tesoro                             
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino     


camino1 = (Cofre [Cacharro, Cacharro])(Nada (Nada Fin))
camino2 = (Cofre [Cacharro, Cacharro])(Nada (Nada (Cofre [Cacharro,Tesoro]Fin)))
camino3 = (Cofre [Cacharro, Cacharro, Tesoro])(Nada (Nada (Cofre [Cacharro,Tesoro]Fin)))
camino4 = (Cofre [Cacharro, Cacharro] (Cofre [Tesoro](Nada (Nada (Cofre [Cacharro,Tesoro]Fin)))))
camino5 = (Cofre [Cacharro, Tesoro])(Nada (Nada Fin))
hayTesoro :: Camino -> Bool
hayTesoro Fin               = False
hayTesoro (Nada cam )       = False  || hayTesoro cam
hayTesoro (Cofre elems c)   = contieneTesoro elems || hayTesoro c        

esTesoro:: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

contieneTesoro:: [Objeto] -> Bool
contieneTesoro []       = False
contieneTesoro (x:xs)   = esTesoro x || contieneTesoro xs    

pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin                = 0
pasosHastaTesoro (Nada cam)         = 1 + pasosHastaTesoro cam
pasosHastaTesoro (Cofre elem cam)   = if (contieneTesoro elem)
                                        then 0
                                        else 1 + pasosHastaTesoro cam      

hayTesoroEn:: Int -> Camino -> Bool
hayTesoroEn n Fin               = False
hayTesoroEn 0 (Nada cam)        = False
hayTesoroEn n (Nada cam)        = hayTesoroEn (n - 1) cam
hayTesoroEn n (Cofre elem cam)  = if (n > 0) 
                                        then hayTesoroEn (n - 1) cam 
                                        else contieneTesoro elem

cantTesorosPorCofre:: [Objeto] -> Int
cantTesorosPorCofre []       = 0
cantTesorosPorCofre (x:xs)   = if(esTesoro x)
                                then 1 + cantTesorosPorCofre xs
                                else cantTesorosPorCofre xs

cantTesorosTotal:: Camino -> Int
cantTesorosTotal Fin                 = 0
cantTesorosTotal (Nada cam)          = cantTesorosTotal cam
cantTesorosTotal (Cofre elem cam)    = (cantTesorosPorCofre elem) + cantTesorosTotal cam

--Indica si hay al menos “n” tesoros en el camino.
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n unCamino = n <= (cantTesorosTotal unCamino)

--Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
--el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
--incluidos tanto 3 como 5 en el resultado
cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre 0 hastaPasos camino =
    cantTesorosEn hastaPasos camino
cantTesorosEntre caminarPasos hastaPasos (Cofre _ resto) =
    cantTesorosEntre (caminarPasos -1) (hastaPasos -1) resto 
cantTesorosEntre caminarPasos hastaPasos (Nada resto) =
    cantTesorosEntre (caminarPasos -1) (hastaPasos - 1) resto 

cantTesorosEn :: Int -> Camino -> Int
cantTesorosEn 0 Fin = 0
cantTesorosEn 0 (Cofre objetos resto) = cantTesorosPorCofre objetos
cantTesorosEn 0 (Nada resto) = 0
cantTesorosEn n (Cofre objetos resto) = cantTesorosPorCofre objetos +
     (cantTesorosEn (n-1) resto)
cantTesorosEn n (Nada resto) = (cantTesorosEn (n-1) resto)



--2.1. Árboles binario

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

arbol1:: Tree Int
arbol1 = NodeT 10
            (NodeT 12
                EmptyT
                EmptyT)




            (NodeT 20
                (NodeT 30
                    EmptyT
                    EmptyT)
                (NodeT 40
                    EmptyT
                    (NodeT 20
                        EmptyT
                        EmptyT)
                )
            )

arbol2 :: Tree Int
arbol2 = NodeT 51
            (NodeT 12
                (NodeT 1
                    EmptyT
                    EmptyT)
                (NodeT 43
                    (NodeT 36
                        EmptyT
                        EmptyT)
                    EmptyT))
            (NodeT 87
                (NodeT 52
                    EmptyT
                    (NodeT 83
                        EmptyT
                        EmptyT))
            EmptyT )
                        
------------------------------------------------------------
arbol3 :: Tree Int
arbol3 = NodeT 51
            (NodeT 12
                (NodeT 1
                    EmptyT
                    EmptyT)
                (NodeT 43
                    (NodeT 36
                        (NodeT 100
                            EmptyT
                            EmptyT)
                        EmptyT)
                    EmptyT))
            (NodeT 87
                (NodeT 52
                    EmptyT
                    (NodeT 83
                        EmptyT
                        EmptyT))
            EmptyT )            

--------------------------------------------------------------------------
arbol4 :: Tree Int
arbol4 = NodeT 51
            (NodeT 12
                (NodeT 1
                    EmptyT
                    EmptyT)
                (NodeT 43
                    (NodeT 36
                        EmptyT
                        EmptyT)
                    EmptyT))
            (NodeT 87
                (NodeT 52
                    EmptyT
                    (NodeT 83
                        EmptyT
                        (NodeT 150
                            EmptyT
                            EmptyT)))
            EmptyT )

-- 1 Dado un árbol binario de enteros devuelve la suma entre sus elementos
sumarT :: Tree Int -> Int
sumarT EmptyT           = 0
sumarT (NodeT e ti td)  = e + sumarT ti + sumarT td      

--Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
--en inglés).
sizeT :: Tree a -> Int
sizeT EmptyT            = 0
sizeT (NodeT e ti td)   = 1 + sizeT ti + sizeT td

doble:: Int -> Int
doble x = x * 2

--Dado un árbol de enteros devuelve un árbol con el doble de cada número.
mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT            = EmptyT
mapDobleT (NodeT e ti td)   = (NodeT (doble e) (mapDobleT ti) (mapDobleT td))

--Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el árbol
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT x EmptyT             = False
perteneceT x (NodeT e ti td)    = x == e || perteneceT x ti || perteneceT x td 

--Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
--iguales a e
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT x EmptyT           = 0
aparicionesT x (NodeT e ti td) = if (x == e)
                                    then 1 + (aparicionesT x ti) + (aparicionesT x td)
                                    else  (aparicionesT x ti) + (aparicionesT x td)

--Dado un árbol devuelve los elementos que se encuentran en sus hojas.
leaves :: Tree a -> [a]
leaves EmptyT                   = []
leaves (NodeT x EmptyT EmptyT)  = x : []    
leaves (NodeT e ti td)          = leaves ti ++ leaves td


--Dado un árbol devuelve su altura.
--Nota: la altura de un árbol (height en inglés), también llamada profundidad, es la cantidad
--de niveles del árbol1. La altura para EmptyT es 0, y para una hoja es 1.
heightT :: Tree a -> Int
heightT EmptyT           = 0
heightT (NodeT e ti td)  = 1 + max (heightT ti)  (heightT td)

--Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho,
--en cada nodo del árbol.
mirrorT :: Tree a -> Tree a
mirrorT EmptyT          = EmptyT
mirrorT (NodeT e ti td) = NodeT e (mirrorT td) (mirrorT ti)

{-Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz
y luego los elementos del hijo derecho-}
toList :: Tree a -> [a]
toList EmptyT           = []
toList (NodeT e ti td)  = toList ti ++ [e] ++ toList td

{-Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un
nodo es la distancia que hay de la raíz hasta él. La distancia de la raiz a sí misma es 0, y la
distancia de la raiz a uno de sus hijos es 1.
Nota: El primer nivel de un árbol (su raíz) es 0 -}
levelN :: Int -> Tree a -> [a]
levelN n EmptyT = [] 
levelN 0 (NodeT x ti td) = x : []
levelN n (NodeT x ti td) = levelN (n-1) ti ++ levelN (n-1) td

--Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de
--dicho árbol.

zipListas :: [[a]] -> [[a]] ->[[a]]
zipListas [] yss = yss
zipListas xss [] = xss
zipListas (xs: xss) (ys:yss) = (xs ++ ys) : (zipListas xss yss)

--Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de dicho árbol.
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT x ti td) = [x] : (zipListas (listPerLevel ti) (listPerLevel td))

--Devuelve los elementos de la rama más larga del árbol
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT x ti td) = if heightT ti > heightT td 
                                    then x : ramaMasLarga ti
                                    else x : ramaMasLarga td
--Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raiz hasta las hojas
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos(NodeT x EmptyT EmptyT) = [[x]]
todosLosCaminos (NodeT x ti td) =  (caminoA x (todosLosCaminos ti  ++  todosLosCaminos td))



caminoA :: a -> [[a]] -> [[a]]
caminoA x []= []
caminoA x (xs:xss) = (x : xs) : (caminoA x xss)             
























