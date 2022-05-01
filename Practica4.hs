data Pizza = Prepizza
             | Capa Ingrediente Pizza  

data Ingrediente = Salsa              
                   | Queso
                   | Jamon
                   | Aceitunas Int   



dobleQueso = (Capa Queso(Capa Queso (Capa Salsa Prepizza)))
dobleJyQ  = (Capa Queso(Capa Jamon(Capa Queso(Capa Jamon Prepizza))))
--Dada una pizza devuelve la cantidad de ingredientes
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza     = 0
cantidadDeCapas (Capa ing p) = 1 +  cantidadDeCapas p

--Dada una lista de ingredientes construye una pizza
armarPizza :: [Ingrediente] -> Pizza
armarPizza []     = Prepizza
armarPizza (x:xs) = Capa x (armarPizza xs)


--Le saca los ingredientes que sean jamón a la pizza
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza     = Prepizza     
sacarJamon (Capa ing p) = if (esJamon ing)
                            then sacarJamon p
                            else (Capa ing (sacarJamon p))

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _     = False

--Dice si una pizza tiene salsa y queso
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza     = True
tieneSoloSalsaYQueso (Capa ing p) = (esSalsaOQueso ing) && tieneSoloSalsaYQueso p

esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso Queso = True
esSalsaOQueso Salsa = True
esSalsaOQueso _     = False

--Recorre cada ingrediente y si es aceitunas duplica su cantidad
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza     = Prepizza
duplicarAceitunas (Capa ing p) =  if esAceituna ing   
                                    then Capa ing (Capa ing (duplicarAceitunas p)) 
                                    else Capa ing (duplicarAceitunas p)

esAceituna :: Ingrediente -> Bool
esAceituna (Aceitunas _) = True
esAceituna _             = False

--Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de
--ingredientes de la pizza, y la respectiva pizza como segunda componente.
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza []     = []
cantCapasPorPizza (p:ps) = (cantidadDeCapas p, p) : (cantCapasPorPizza ps)


data Dir = Izq | Der               
data Objeto = Tesoro | Chatarra      
data Cofre  = Cofre [Objeto]         
data Mapa   = Fin Cofre               
            | Bifurcacion Cofre Mapa Mapa   

mapa1 :: Mapa
mapa1 = Fin (Cofre [Tesoro])

mapa2 :: Mapa
mapa2 = 
  Bifurcacion (Cofre [Chatarra])
              (Fin (Cofre [Tesoro]))
              (Fin (Cofre [Chatarra]))

mapa3 :: Mapa
mapa3 = Bifurcacion (Cofre [Chatarra]) 
                    mapa1
                    mapa1

mapa4 :: Mapa
mapa4 = Bifurcacion (Cofre [Tesoro])
                    mapa3
                    mapa3

mapa5 :: Mapa
mapa5 = Bifurcacion (Cofre[Chatarra])
                    (Fin(Cofre[Tesoro,Tesoro,Tesoro]))
                    (Bifurcacion(Cofre[Tesoro,Tesoro])
                                        (Fin(Cofre[]))
                                        (Bifurcacion(Cofre[])
                                                              (Fin(Cofre[Tesoro]))
                                                              (Fin(Cofre[Tesoro,Tesoro,Tesoro,Tesoro,Tesoro,Tesoro]))))
mapa6 :: Mapa
mapa6 = Bifurcacion (Cofre[])
                    mapa5
                    (Fin(Cofre[]))                                                              
--Indica si hay un tesoro en alguna parte del mapa
hayTesoro :: Mapa -> Bool
hayTesoro (Fin c)               = tieneTesoro c
hayTesoro (Bifurcacion c m1 m2) = (tieneTesoro c) || hayTesoro m1 || hayTesoro m2

tieneTesoro:: Cofre -> Bool
tieneTesoro (Cofre elems) = algunTesoro elems

algunTesoro :: [Objeto] -> Bool
algunTesoro []     = False 
algunTesoro (x:xs) = (esTesoro x) || algunTesoro xs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

--Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una
--lista vacía de direcciones.
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] (Fin c)                 = tieneTesoro c
hayTesoroEn [] (Bifurcacion c m1 m2)   = tieneTesoro c
hayTesoroEn (d:ds) (Fin c)             = error "se termino el camino y la lista no"
hayTesoroEn (d:ds) (Bifurcacion c m1 m2) = if esIzq d
                                         then hayTesoroEn ds m1
                                         else hayTesoroEn ds m2


esIzq :: Dir -> Bool
esIzq Izq = True
esIzq _   = False


heightM :: Mapa -> Int
heightM (Fin(Cofre cosas)) = 0
heightM (Bifurcacion (Cofre cosas) mp1 mp2) = 1 + max (heightM mp1) (heightM mp2)


--Indica el camino de la rama más larga.
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin (Cofre cosas)) = []
caminoDeLaRamaMasLarga (Bifurcacion(Cofre cosas) mp1 mp2) = if (heightM mp1) > (heightM mp2)
                                                               then Izq : caminoDeLaRamaMasLarga mp1
                                                               else Der: caminoDeLaRamaMasLarga mp2

tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin (Cofre cosas)) = [cosas]
tesorosPorNivel (Bifurcacion(Cofre cosas) mp1 mp2) = [cosas] ++ (zipListas(tesorosPorNivel mp1)(tesorosPorNivel mp2))

zipListas :: [[a]] -> [[a]] ->[[a]]
zipListas [] yss = yss
zipListas xss [] = xss
zipListas (xs: xss) (ys:yss) = (xs ++ ys) : (zipListas xss yss)

--Devuelve todos lo caminos en el mapa.
caminoA :: a -> [[a]] -> [[a]]
caminoA x []= [[x]]
caminoA x (xs:xss) = (x : xs) : (caminoA x xss)

todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin(Cofre cosas)) = []
todosLosCaminos (Bifurcacion(Cofre(cosas)) mp1 mp2) = (caminoA Izq(todosLosCaminos mp1)) ++ (caminoA Der(todosLosCaminos mp2)) 
--------------------------------------------------------------------------------------------------------------------------------------
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]  

data Barril = Comida | Oxigeno | Torpedo | Combustible   

data Sector = S SectorId [Componente] [Tripulante]   

type SectorId = String
type Tripulante = String

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) 
data Nave = N (Tree Sector) 


compAlmacen = Almacen [Comida,Oxigeno,Torpedo]

sectorCabina = S "cabina" [LanzaTorpedos, Motor 20, compAlmacen] ["Luis","Juan"]

sectorSala1 = S "sala1" [Almacen [Comida,Oxigeno, Oxigeno]] ["Pepe", "Mario"]

sectorSala2 = S "sala2" [LanzaTorpedos, Motor 50, Motor 30] ["Kurt", "Dave"]

sectorSala3 = S "sala3" [Almacen [Combustible,Combustible],LanzaTorpedos, Almacen [Oxigeno,Oxigeno]] ["Tito", "Sheena"]

nav :: Nave
nav = N (nave1)

nave1:: Tree Sector
nave1 = NodeT sectorCabina 
                (NodeT sectorSala1
                  EmptyT
                  EmptyT)
                (NodeT sectorSala3
                  EmptyT
                  EmptyT)

--Propósito: Devuelve todos los sectores de la nave.
sectores :: Nave -> [SectorId] --Nave es N (Tree Sector)
sectores (N t) = sectoresT t  -- Agarro los sectores del arbol

sectoresT :: Tree Sector -> [SectorId] --Arbol de sectores
sectoresT EmptyT = []
sectoresT (NodeT x ti td) = idS x :  sectoresT ti ++ sectoresT td

idS :: Sector -> SectorId
idS (S s comp trip) = s      --Hago pattern matching y me quedo con el sectorId
----------------------------------------------------------------------------------------------------------------------------------

--Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota:
--el poder de propulsión es el número que acompaña al constructor de motores.
poderDePropulsion :: Nave -> Int
poderDePropulsion (N t) = sumaDeMotoresTotal (todosLosComponentes t)

--Me quedo con la lista de listas de componentes de todos los sectores
todosLosComponentes :: Tree Sector -> [[Componente]]
todosLosComponentes EmptyT = []
todosLosComponentes (NodeT x ti td) = componentes x : todosLosComponentes ti ++ todosLosComponentes td

--Me quedo con la lista de componentes de un sector
componentes :: Sector -> [Componente]
componentes (S s comp trip ) = comp

--Me quedo con el valor del motor 
potencia :: Componente -> Int
potencia (Motor n) = n


--Devuelvo true si es motor
esMotor :: Componente -> Bool
esMotor (Motor _) = True
esMotor _ = False

--Chequeo  cada componente, si es un motor sumo el valor de cada motor
sumaDeMotores :: [Componente] -> Int
sumaDeMotores [] = 0
sumaDeMotores (x:xs) = if esMotor x 
                        then potencia x +  sumaDeMotores xs
                        else sumaDeMotores xs



sumaDeMotoresTotal :: [[Componente]] -> Int
sumaDeMotoresTotal [] = 0
sumaDeMotoresTotal (xs: xss) = sumaDeMotores xs + sumaDeMotoresTotal xss

