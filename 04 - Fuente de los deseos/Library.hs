module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero


-- Creo el tipo de dato Persona

data Persona = UnaPersona { -- UnaPersona es una función constructor
    edad :: Number,
    nombre :: String,
    felicidonios :: Number,
    suenios :: [String]
} deriving Showe

-- Coeficiente de satisfacción

coeficienteSat :: Persona -> Number
coeficienteSat persona
    | felicidonios persona > 100 = felicidonios persona * edad persona
    | felicidonios persona <= 100 && felicidonios persona > 50 = felicidonios  persona * length (suenios persona)
    | otherwise = 40


-- Nombre largo
esLargo :: Persona -> Bool
esLargo persona
    | length (nombre persona) > 10 = True
    | otherwise = False


-- Persona suertuda

personaSuertuda :: Persona -> Bool
personaSuertuda persona
    | (coeficienteSat persona) > 10 = True
    | otherwise = False


-- Nombre lindo

nombreLindo :: Persona -> Bool
nombreLindo persona
    | head (nombre persona) == 'L' = True
    | otherwise = False


-- Cumplir sueños

cumplirSuenios :: Persona -> Persona
cumplirSuenios persona = persona { edad = edad persona,

                                   nombre = nombre persona,

                                   felicidonios = coeficienteSat persona * length (suenios persona) + felicidonios persona,

                                   suenios = drop (length (suenios persona)) (suenios persona)
                                
                                  }

-- Fuente de los deseos


tirarMoneda1 :: Persona -> Persona
tirarMoneda1 persona =  persona {edad = edad persona,

                            nombre = nombre persona,

                            felicidonios =  felicidonios persona + 10,

                            suenios = suenios persona}


tirarMoneda2 :: Persona -> Persona
tirarMoneda2 persona

    | personaSuertuda persona == True = cumplirSuenios persona
                                  
    | otherwise persona


tirarMoneda :: Persona -> Persona
tirarMoneda persona = tirarMoneda1 (tirarMoneda2 persona)
 
