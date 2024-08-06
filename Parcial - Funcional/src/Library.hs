module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- 1 

data Nave = UnaNave {

nombre :: String,

durabilidad :: Number,

escudo :: Number,

ataque :: Number,

nombrepoder :: String, -- decidí agregar el nombre del poder al data de Nave como un String para poder leer qué poder tiene cada Nave por consola,
                       -- ya que poder, por si solo, se muestra como <una función> en consola (y tiene sentido porque lo definí como Nave -> Nave).

poder :: Poder


} deriving Show

type Poder = Nave -> Nave

type Flota = [Nave]

tiefighter :: Nave
tiefighter = UnaNave {nombre = "Tie Fighter", durabilidad =  200, escudo = 100, ataque = 50, nombrepoder = "Turbo", poder = turbo}

xwing :: Nave
xwing = UnaNave {nombre = "X Wing", durabilidad = 300, escudo = 150, ataque = 100, nombrepoder = "Reparacion de emergencia", poder = reparacionEmergencia}

naveDarthVader :: Nave
naveDarthVader = UnaNave {nombre = "Nave de Darth Vader", durabilidad = 500, escudo = 300, ataque = 200, nombrepoder = "Super turbo", poder = superTurbo}

milleniumFalcon :: Nave
milleniumFalcon = UnaNave {nombre = "Millennium Falcon", durabilidad = 1000, escudo = 500, ataque = 50, nombrepoder = "Poder Milenario", poder = poderMilenario}

naveInia :: Nave
naveInia = UnaNave {nombre = "Nave Inia", durabilidad = 1000, escudo = 500, ataque = 500, nombrepoder = "Super turboreparacion", poder = superTurboreparacion} -- dos turbos e incrementar escudos en 100

turbo :: Poder
turbo nave = incrementarAtaque 25 nave

reparacionEmergencia :: Poder
reparacionEmergencia nave  = (incrementarDurabilidad 50 . reducirAtaque 30) nave

superTurbo :: Poder
superTurbo nave = ((repetir 3 [turbo]) . (reducirDurabilidadEn 45)) nave 

poderMilenario :: Poder
poderMilenario nave = (reparacionEmergencia . incrementarEscudos 100) nave

superTurboreparacion :: Poder
superTurboreparacion nave = ((repetir 2 [turbo]) . (incrementarEscudos 100)) nave

incrementarAtaque :: Number -> Nave -> Nave
incrementarAtaque cantidad nave = UnaNave {nombre = nombre nave, durabilidad = durabilidad nave, escudo = escudo nave, ataque = ataque nave + cantidad, nombrepoder = nombrepoder nave, poder = poder nave}

incrementarDurabilidad :: Number -> Nave -> Nave
incrementarDurabilidad cantidad nave = UnaNave {nombre = nombre nave, durabilidad = durabilidad nave + cantidad, escudo = escudo nave, ataque = ataque nave, nombrepoder = nombrepoder nave, poder = poder nave}

reducirAtaque :: Number -> Nave -> Nave
reducirAtaque cantidad nave = UnaNave {nombre = nombre nave, durabilidad = durabilidad nave, escudo = escudo nave, ataque = ataque nave - cantidad, nombrepoder = nombrepoder nave, poder = poder nave}

incrementarEscudos :: Number -> Nave -> Nave
incrementarEscudos cantidad nave =  UnaNave {nombre = nombre nave, durabilidad = durabilidad nave, escudo = escudo nave + cantidad, ataque = ataque nave, nombrepoder = nombrepoder nave, poder = poder nave}

reducirDurabilidadEn :: Number -> Nave -> Nave
reducirDurabilidadEn cantidad nave = UnaNave {nombre = nombre nave, durabilidad = (durabilidad nave - cantidad) `min` 0, escudo = escudo nave, ataque = ataque nave, nombrepoder = nombrepoder nave, poder = poder nave}

repetir :: Number -> [Poder] -> Poder
repetir 0 [_] = id
repetir cantidad poderes = repetir (cantidad - 1) poderes

-- 2 

durabilidadTotal :: Flota -> Number
durabilidadTotal [] = 0
durabilidadTotal flota = sum (map sacarDurabilidad flota)

sacarDurabilidad :: Nave -> Number
sacarDurabilidad = durabilidad

-- 3 

naveLuegoDeAtaque :: Nave -> Nave -> Nave
naveLuegoDeAtaque naveatacada navequeataca 
    | laAfectaElAtaque naveatacada navequeataca = reducirDurabilidadEn (ataque (activarPoderEspecial navequeataca)) (activarPoderEspecial naveatacada)
    | otherwise = naveatacada

laAfectaElAtaque :: Nave -> Nave -> Bool
laAfectaElAtaque naveatacada navequeataca = escudo (activarPoderEspecial naveatacada) < ataque (activarPoderEspecial navequeataca)

activarPoderEspecial :: Nave -> Nave
activarPoderEspecial nave = (poder nave) nave

-- 4 

estaFueraDeCombate :: Nave -> Bool
estaFueraDeCombate nave = durabilidad nave == 0

-- 5

type Estrategia = Nave -> Bool

flotaLuegodeAtaque :: Nave -> Flota -> Flota
flotaLuegodeAtaque nave flota = map (naveLuegoDeAtaque nave) flota

flotaLuegodeMision :: Nave -> Flota -> Estrategia -> Flota
flotaLuegodeMision nave flota estrategia = map (atacarSiCumple estrategia nave) flota

atacarSiCumple :: Estrategia -> Nave -> Nave -> Nave
atacarSiCumple estrategia navequeataca naveatacada
    | estrategia naveatacada = naveLuegoDeAtaque navequeataca naveatacada
    | otherwise = naveatacada

esDebil :: Estrategia
esDebil nave = escudo nave < 200

tieneCiertaPeligrosidad :: Nave -> Number -> Bool
tieneCiertaPeligrosidad nave peligrosidad = ataque nave > peligrosidad

fueraDeCombate :: Estrategia
fueraDeCombate nave = estaFueraDeCombate nave

dura :: Estrategia -- nueva estrategia, la ataco si la durabilidad es mayor a 200 y si el escudo es mayor a 200.
dura nave = (durabilidad nave > 200) && (escudo nave > 200)


-- 6

flotaLuegodeMisionConEstrategiaMasApta ::  Nave -> Flota -> Estrategia -> Estrategia -> Flota
flotaLuegodeMisionConEstrategiaMasApta nave flota estrategia1 estrategia2 = flotaLuegodeMision nave flota (estrategiaMasApta nave flota estrategia1 estrategia2)

estrategiaMasApta :: Nave -> Flota -> Estrategia -> Estrategia -> Estrategia
estrategiaMasApta nave flota estrategia1 estrategia2
    | durabilidadTotal (flotaLuegodeMision nave flota estrategia1) > durabilidadTotal (flotaLuegodeMision nave flota estrategia2) = estrategia2
    | otherwise = estrategia1


-- 7. Construir una flota infinita de naves.

flotaInfinita :: Flota
flotaInfinita = repeat naveInia

-- ¿Es posible determinar su durabilidad total? 

-- No, no es posible, el map de durabilidadTotal se queda "colgado" recorriendo los elementos de la lista, que es infinita,
-- por lo tanto el sum de durabilidadTotal tiene que sumar una cantidad infinitas de elementos, lo cuale es imposible


-- ¿Qué se obtiene como respuesta cuando se lleva adelante una misión sobre ella?

-- Para llevar a cabo una misión sobre una flota, necesitamos evaluar si la nave atacante va a atacar o no a cada una de las naves de la flota;
-- en el caso de la flota infinita, esta evaluación no tiene fin, ya que estamos pidiendo al programa que evalúe si va a atacar o no a una cantidad
-- infinita de elementos. No devuelve nada, se queda nuevamente "colgado", evualuando infinitamente.
