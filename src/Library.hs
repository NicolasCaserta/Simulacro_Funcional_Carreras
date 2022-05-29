module Library where
import PdePreludat

data Auto = UnAuto{
    color :: String,
    velocidad :: Number,
    distanciaRecorrida :: Number
    }deriving(Show,Eq)

type Carrera = [Auto]
type PowerUp = Auto -> Carrera -> Carrera
type Color = String

-------------------------------------------------------------- PUNTO 1 --------------------------------------------------------------

autoEstaCerca :: Auto -> Auto -> Bool
autoEstaCerca auto1 auto2 = (auto1 /= auto2) && (abs (distanciaRecorrida auto1 - distanciaRecorrida auto2) < 10)

autoVaTranquilo :: Auto -> Carrera -> Bool
autoVaTranquilo auto carrera = all (== False) (map (autoEstaCerca auto) carrera) 
                                                && (distanciaRecorrida auto == maximum (map (distanciaRecorrida) carrera))

puesto :: Auto -> Carrera -> Number
puesto auto carrera = 1 + length (filter (> distanciaRecorrida auto) (map distanciaRecorrida carrera))

-------------------------------------------------------------- PUNTO 2 --------------------------------------------------------------

correrDeterminadoTiempo :: Number -> Auto -> Auto
correrDeterminadoTiempo tiempo auto = auto {distanciaRecorrida = distanciaRecorrida auto + (velocidad auto) * tiempo}

alterarVelocidad :: (Number -> Number) -> Auto -> Auto
alterarVelocidad modificador auto = auto {velocidad = modificador (velocidad auto)}

ajustarVelocidad :: (Number -> Number) -> Auto -> Auto
ajustarVelocidad modificador auto | (modificador (velocidad auto)) <0 = auto {velocidad=0}
                                  | otherwise = auto {velocidad = modificador (velocidad auto)}

-------------------------------------------------------------- PUNTO 3 --------------------------------------------------------------

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

terremoto :: PowerUp
terremoto auto carrera = afectarALosQueCumplen (autoEstaCerca auto) (ajustarVelocidad (+(-50))) carrera

miguelitos :: Number -> PowerUp
miguelitos cant auto carrera = afectarALosQueCumplen (vaGanando auto) (ajustarVelocidad (+(-cant))) carrera
vaGanando :: Auto -> Auto -> Bool
vaGanando auto1 auto2 = distanciaRecorrida auto1 > distanciaRecorrida auto2

jetPack :: Number -> PowerUp
jetPack tiempo auto carrera = afectarALosQueCumplen (==auto) (alterarVelocidad (/2) . correrDeterminadoTiempo tiempo . alterarVelocidad (*2)) carrera

-------------------------------------------------------------- PUNTO 4 --------------------------------------------------------------

simularCarrera :: Carrera -> [Carrera -> Carrera] -> [(Number, Color)]
simularCarrera carrera eventos = (tablaDePosiciones . foldl (flip ($)) carrera) eventos

tablaDePosiciones :: Carrera -> [(Number, Color)]
tablaDePosiciones carrera = zip (map (flip puesto carrera) carrera) (map color carrera)

correnTodos :: Number -> (Carrera -> Carrera)
correnTodos tiempo = map (correrDeterminadoTiempo tiempo)

usaPowerUp :: Color -> PowerUp -> (Carrera -> Carrera)
usaPowerUp colorAuto powerUp carrera = flip powerUp carrera . buscarAuto colorAuto $ carrera

buscarAuto :: String -> Carrera -> Auto
buscarAuto colorAuto = head . filter ((== colorAuto) . color)

carreraDeEjemplo :: Carrera
carreraDeEjemplo = map (\color -> UnAuto color 120 0) [ "rojo", "blanco", "azul", "negro"]

eventosDeEjemplo = [ 
                    correnTodos 30,
                    "azul" `usaPowerUp` (jetPack 3),
                    "blanco" `usaPowerUp` terremoto,
                    correnTodos 40,
                    "blanco" `usaPowerUp` (miguelitos 20),
                    "negro" `usaPowerUp` (jetPack 6),
                    correnTodos 10
                   ]

-- Ejemplo --
camaro = UnAuto {color="Blnaco", velocidad=10, distanciaRecorrida=10}

-------------------------------------------------------------- PUNTO 5 --------------------------------------------------------------

{-
 --- 5a ---

Se puede agregar sin problemas como una función más:
misilDirigido :: Color -> Carrera -> Carrera
misilDirigido objetivo = afectarALosQueCumplen (\auto -> color auto == objetivo) (ajustarVelocidad (*0))

y usarlo como:
usaPowerUp "azul" (misilTeledirigido "rojo")

 --- 5b ---

- vaTranquilo puede terminar sólo si el auto indicado no va tranquilo (en este caso por tener a alguien cerca, si las condiciones 
                                                                estuvieran al revés, terminaría si se encuentra alguno al que no le gana).

Esto es gracias a la evaluación perezosa, any es capaz de retornar True si se encuentra alguno que cumpla la condición indicada, y all
es capaz de retornar False si alguno no cumple la condición correspondiente. Sin embargo, no podría terminra si se tratara de un auto 
                                                            que va tranquilo.

- puesto no puede terminar nunca porque hace falta saber cuántos le van ganando, entonces por más que se pueda tratar de filtrar el 
                            conjunto de autos, nunca se llegaría al final para calcular la longitud.
-}