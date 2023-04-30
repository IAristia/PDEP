-- ARISTIA, Iñaki - Legajo: 176.382-9 - PdeP - K2104 - 2023 - TP 01 Empresas

module Library where
import PdePreludat
    ( otherwise,
      Eq((==)),
      Ord((>)),
      String,
      (+),
      Number,
      reverse,
      (&&),
      (||),
      fromInteger,
      (*),
      (-),
      even,
      head,
      last,
      length,
      mod )

doble :: Number -> Number
doble numero = numero + numero

-- Función para calcular la cantidad de empleados contratados por sucursal por una empresa dado el nombre de la empresa.

cantEmpleadosxSucu :: String -> Number
cantEmpleadosxSucu nombreEmpresa

    | nombreEmpresa == "Acme" = 10

    | head nombreEmpresa >  last nombreEmpresa = length nombreEmpresa - 2 -- considera los valores de la tabla ASCII https://elcodigoascii.com.ar

    | nombreEmpresa == reverse nombreEmpresa && even (length nombreEmpresa) = (length nombreEmpresa - 2) * 2

    | mod (length nombreEmpresa) 3 == 0  || mod (length nombreEmpresa) 7 == 0 = 3

    | otherwise = 0

-- Función para calcular la cantidad de empleados contratados en total por una empresa dado el nombre de la empresa y la cantidad de sucursales a abrir.

cantEmpleadosTotal :: String -> Number -> Number
cantEmpleadosTotal nombreEmpresa cantSucursales

    | nombreEmpresa == "Acme" = 10 * cantSucursales

    | head nombreEmpresa >  last nombreEmpresa = (length nombreEmpresa - 2) * cantSucursales -- considera los valores de la tabla ASCII https://elcodigoascii.com.ar

    | nombreEmpresa == reverse nombreEmpresa && even (length nombreEmpresa) = (length nombreEmpresa - 2) * 2 * cantSucursales
    
    | mod (length nombreEmpresa) 3 == 0  || mod (length nombreEmpresa) 7 == 0 = 3 * cantSucursales

    | otherwise = 0
