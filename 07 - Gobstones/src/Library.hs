module Library where
import PdePreludat
import Data.List (sort)

doble :: Number -> Number
doble numero = numero + numero

-- Punto 1

type Fila = Number

type Columna = Number

type Coordenada = (Fila, Columna)

type Matriz = [Coordenada]

type Color = String

type Cantidad = Number

type Cabezal = Coordenada 

type Bolita = (Color,Cantidad)
data Celda = UnaCelda {
    coordenada :: Coordenada,
    bolitas :: [Bolita]
}deriving (Show,Eq,Ord)

data Tablero = UnTablero {

    celdas :: [Celda],
    cabezal :: Cabezal 
}deriving (Show,Eq,Ord)


inicializarMatriz :: Fila -> Columna -> Matriz
inicializarMatriz cantFilas cantColumnas = [(x, y)|x <-[1..cantFilas],y <-[1..cantColumnas]]
{- 
agregarCantidadBolitas :: Coordenada -> Celda
agregarCantidadBolitas coordenadaMatriz = UnaCelda {coordenada = coordenadaMatriz, bolitas = [("Rojo", 0) ,("Verde", 0), ("Negro", 0), ("Azul", 0)]} -}

agregarCantidadBolitas :: Coordenada -> Celda
agregarCantidadBolitas coordenadaMatriz = UnaCelda coordenadaMatriz [("Rojo", 0) ,("Verde", 0), ("Negro", 0), ("Azul", 0)]

matrizATablero :: Matriz -> [Celda]
matrizATablero matriz = map agregarCantidadBolitas  matriz

inicializarTablero :: Fila -> Columna -> Tablero
inicializarTablero fila columna = UnTablero (matrizATablero (inicializarMatriz fila columna)) (inicializarCabezal (inicializarMatriz fila columna))

inicializarCabezal :: Matriz -> Cabezal
inicializarCabezal matriz = head matriz

tableroEjemplo :: Tablero
tableroEjemplo = inicializarTablero 2 2

tableroEjemplo2 :: Tablero
tableroEjemplo2 = UnTablero{celdas = matrizATablero(inicializarMatriz 3 3),cabezal = (2,2)}

type Direccion = String

norte :: Direccion
norte = "Norte"
sur :: Direccion
sur = "Sur"
este :: Direccion
este = "Este"
oeste :: Direccion
oeste = "Oeste"

-- Punto 3
mover :: Direccion -> Tablero -> Tablero
mover direccion tablero = tablero{cabezal = moverEnDireccion (cabezal tablero) direccion}

moverEnDireccion :: Cabezal -> Direccion -> Cabezal
moverEnDireccion (x,y) direccion
    | direccion == norte = (x+1, y)
    | direccion == sur = (x-1,y)
    | direccion == oeste = (x,y-1)
    | direccion == este = (x,y+1)
    | otherwise = (x,y)

poner :: Color -> Tablero -> Tablero
poner color tablero = UnTablero{celdas = sort((:) (sumarBolitaACelda color tablero) (quitarCeldaAActualizar tablero)),cabezal = cabezal tablero}

sacar :: Color -> Tablero -> Tablero
sacar color tablero = UnTablero{celdas = sort((:) (restarBolitaACelda color tablero) (quitarCeldaAActualizar tablero)),cabezal = cabezal tablero}

sumarBolitaACelda :: Color -> Tablero -> Celda
sumarBolitaACelda color tablero = UnaCelda{coordenada = cabezal tablero,
bolitas = actualizarListaBolitas color (sumarBolita(buscarBolita(buscarCelda(cabezal tablero) (celdas tablero)) color)) (bolitas(buscarCelda(cabezal tablero) (celdas tablero)))}

restarBolitaACelda :: Color -> Tablero -> Celda
restarBolitaACelda color tablero = UnaCelda{coordenada = cabezal tablero,
bolitas = actualizarListaBolitas color (restarBolita(buscarBolita(buscarCelda(cabezal tablero) (celdas tablero)) color)) (bolitas(buscarCelda(cabezal tablero) (celdas tablero)))}

quitarCeldaAActualizar:: Tablero -> [Celda]
quitarCeldaAActualizar tablero = quitarCelda (buscarCelda (cabezal tablero) (celdas tablero)) (celdas tablero)

quitarCelda :: Celda -> [Celda] -> [Celda]
quitarCelda _ [] = []
quitarCelda x (y:ys)
  | x == y = quitarCelda x ys
  | otherwise = y : quitarCelda x ys

buscarCelda :: Cabezal -> [Celda] -> Celda
buscarCelda _ [] = UnaCelda{coordenada = (-1,-1), bolitas = [("Rojo", -1) ,("Verde", -1), ("Negro", -1), ("Azul", -1)]}
buscarCelda unCabezal (x:xs)
    | unCabezal == coordenada x = x
    | otherwise = buscarCelda unCabezal xs

buscarBolita :: Celda -> Color -> Bolita
buscarBolita celda unColor = head(filter(\(x,_) -> x == unColor) (bolitas celda))

sumarBolita :: Bolita -> Bolita
sumarBolita unaBolita = (fst unaBolita,((1+).snd)unaBolita)

restarBolita :: Bolita -> Bolita
restarBolita unaBolita = (fst unaBolita,snd unaBolita - 1)

actualizarListaBolitas :: Color -> Bolita -> [Bolita] -> [Bolita]
actualizarListaBolitas unColor unaBolita bolitas = (:) unaBolita (filter (\(x,y) -> x /= unColor) bolitas)

-- Punto 4

type Condicion = Tablero -> Bool -- hayBolita, puedeMoverse
type Sentencias = Tablero -> Tablero --Acciones que repercuten en el tablero (Agregar/Sacar Bolita, Moverse)

sentenciaEjemplo :: Tablero -> Tablero
sentenciaEjemplo = poner "Verde"

sentenciaEjemplo2 :: Tablero -> Tablero
sentenciaEjemplo2 = poner "Azul"

sentenciaEjemplo3 :: Tablero -> Tablero
sentenciaEjemplo3 = mover sur

condicionEjemplo :: Tablero -> Bool
condicionEjemplo tablero = sum (map (sum . map snd . bolitas) (celdas tablero)) < 4

aplicarSentencias :: [Sentencias] -> Tablero -> Tablero
aplicarSentencias sentencias tablero = foldl (\tablero f -> f tablero) tablero sentencias

si :: Condicion -> [Sentencias] -> Tablero -> Tablero
si condicion sentencias tablero
            | condicion tablero = aplicarSentencias sentencias tablero
            | otherwise = tablero --Devuelve el tablero como estaba pq no cumple la condicion

siCumple :: Condicion -> Tablero -> Bool
siCumple condicion = condicion 

sino :: Condicion -> [Sentencias] -> Tablero -> Tablero
sino condicion sentencias tablero
            | (not.condicion) tablero= aplicarSentencias sentencias tablero
            | otherwise = tablero --Devuelve el tablero como estaba pq no cumple la condicion

alternativa :: Condicion -> [Sentencias] -> [Sentencias] -> Tablero -> Tablero
alternativa condicion sentencias1 sentencias2 tablero
            | condicion tablero = aplicarSentencias sentencias1 tablero
            | otherwise = aplicarSentencias sentencias2 tablero

repetir :: Cantidad -> [Sentencias] -> Tablero -> Tablero
repetir 0 _ tablero = tablero
repetir cantidad sentencias tablero = repetir (cantidad-1) sentencias (aplicarSentencias sentencias tablero) 

mientras :: Condicion -> [Sentencias] -> Tablero -> Tablero
mientras _ [] tablero = tablero 
mientras unaCondicion sentencias tablero
    | siCumple unaCondicion tablero = mientras unaCondicion sentencias (aplicarSentencias sentencias tablero)
    | otherwise = tablero

irAlBorde :: Direccion -> Tablero -> Tablero
irAlBorde unaDireccion = mientras (puedeMoverse unaDireccion) [mover unaDireccion] 

-- Punto 5

celdaEjemplo :: Celda
celdaEjemplo = UnaCelda (1,1) [("rojo",0),("azul",2),("verde",0),("negro",0)]

puedeMoverse :: Direccion -> Tablero -> Bool
puedeMoverse direccion tablero 
    |direccion == norte && (fst (cabezal tablero) == fst (coordenadaMayor tablero)) = False
    |direccion == sur && fst (cabezal tablero) == 1 = False
    |direccion == oeste && (snd (cabezal tablero) == 1) = False
    |direccion == este && (snd (cabezal tablero) == snd (coordenadaMayor tablero)) = False
    |otherwise = True

coordenadaMayor :: Tablero -> Coordenada
coordenadaMayor = coordenada . last . celdas

hayBolita :: Color -> Tablero -> Bool
hayBolita unColor tablero = any (\(x,y) -> x == unColor && y > 0) (bolitas (buscarCelda (cabezal tablero) (celdas tablero))) 

cantidadBolitas :: Color -> Tablero -> Cantidad
cantidadBolitas unColor tablero = snd(head(filter (\(x,_) -> x == unColor) (bolitas (buscarCelda (cabezal tablero) (celdas tablero)))))

--punto 6
programa :: Tablero -> [Sentencias] -> Tablero
programa tablero sentencias = aplicarSentencias sentencias tablero  

--punto 7
gobstones ::  Tablero -> Tablero
gobstones tablero = aplicarSentencias [mover norte,poner "Negro",poner "Negro",poner "Azul",mover norte,repetir 15 [poner "Rojo", poner "Azul"], alternativa (hayBolita "Verde") [mover este,poner "Negro"] [mover sur,mover este,poner "Azul"], mover este, mientras ((<= 9).cantidadBolitas "Verde") [repetir 10 [poner "Verde"]],poner "Azul"] tablero

tableroGobstones = inicializarTablero 3 3