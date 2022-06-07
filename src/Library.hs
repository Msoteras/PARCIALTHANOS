module Library where
import PdePreludat

--Punto 1

--type Guantelete = (String,[String] )

data Guantelete = UnGuantelete {
    material :: String,
    gemas:: [Gema]
} deriving (Show, Eq)

data Personaje = UnPersonaje {
    edad :: Number,
    energia :: Number,
    habilidades :: [String],
    nombre :: String,
    planeta:: String
} deriving (Show, Eq)

type Universo = [Personaje]

chasquidoDeUnUniverso :: Universo -> Guantelete -> Universo
chasquidoDeUnUniverso universo1 guantelete 
        | material guantelete == "uru" && ((==6).length.gemas) guantelete  = take (((/2).length) universo1) universo1
        | otherwise = universo1

--Punto 2. Resolver con Orden Superior
aptoPendex :: Universo -> Bool
aptoPendex universo1 = any (<45) (edades universo1)

edades :: Universo -> [Number]
edades universo = map edad universo

energiaTotalUniverso :: Universo -> Number
energiaTotalUniverso  = sum.energias.masDeUnaHabilidad

energias :: Universo -> [Number]
energias personajes = map energia personajes --pongo personajes porque no es el universo entero

masDeUnaHabilidad :: Universo -> Universo
masDeUnaHabilidad universo  = filter ((>1).length.habilidades) universo

--Punto 3. Gemas sin repetir logica

type Gema = Personaje -> Personaje
laMente :: Number -> Gema
laMente valor personaje = debilitarEnergia personaje valor -- o directamente sin usar la funcion, solo devuelvo el personaje

debilitarEnergia :: Personaje -> Number -> Personaje
debilitarEnergia personaje valor = personaje {energia = energia personaje - valor}

elAlma :: String -> Gema
elAlma habilidad personaje 
    | elem habilidad (habilidades personaje) = quitarHabilidad (debilitarEnergia personaje 10) habilidad
    | otherwise = debilitarEnergia personaje 10

quitarHabilidad :: Personaje -> String -> Personaje
quitarHabilidad personaje habilidad = personaje {habilidades = filter (/=habilidad) (habilidades personaje)}

elEspacio :: String -> Gema
elEspacio planet personaje = (debilitarEnergia personaje 20){planeta = planet}

elPoder :: Gema
elPoder personaje
    | ((<=2).length.habilidades) personaje = (debilitarEnergia personaje (energia personaje)){habilidades = []}
    | otherwise =  debilitarEnergia personaje (energia personaje)
    
elTiempo :: Gema
elTiempo personaje = (debilitarEnergia personaje 50){edad = max 18 (div (edad personaje) 2)}

laGemaLoca :: Gema -> Gema
laGemaLoca gema = (gema.gema) 

personaje1 = UnPersonaje {edad=18,energia=22,habilidades=["dormir"],nombre ="Yo", planeta ="Luna"}

--Punto 4
guanteleteGoma = UnGuantelete {material="Goma", gemas=[elTiempo, elAlma "usar Mjolnir", laGemaLoca (elAlma "programacion en Haskell")]}

-- Punto 5
utilizar :: [Gema] -> Personaje -> Personaje
utilizar gemas enemigo = foldl (\enemigo gema -> gema enemigo) enemigo gemas

-- Punto 6
gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa guantelete persona = gemaMasdanina persona (gemas guantelete)

gemaMasdanina :: Personaje -> [Gema] -> Gema
gemaMasdanina persona [gema] = gema
gemaMasdanina persona (gema:gemas)
    |  comparoGemas gema (head gemas) persona = gemaMasdanina persona (gema:gemas)
    |  otherwise = gemaMasdanina persona (gemas)

comparoGemas gema1 gema2 persona = ((energia.gema1) persona) > ((energia.gema2) persona)

--Otra Opcion
{-
gemaMasPoderosa guantelete persona = gemaMasdanina persona (gemas guantelete)

gemaMasdanina persona [x] = x
gemaMasdanina persona (x:xs) 
        | all ((aplicarGemaEner persona x)>) (map (aplicarGemaEner persona) xs) = x
        | otherwise = gemaRecursiva personaje xs
        
aplicarGemaEner persona gema = energia (gema personaje) -}

-- Punto 7

{- La funcion gemaMasPoderosa punisher guanteleteDeLocos no se podria ejectuar ya que entrariamos
en un loop infinito donde la funcion estará constantemente buscando cual es la más poderosa, aun
cuando son todas iguales. Estaria siempre ejecutando la parte recursiva donde hasta no comparar
con la ultima gema, no para y no existe en este caso esa ultima

La funcion usoLasTresPrimerasGemas guanteleteDeLocos punisher si podria ejecutarse ya que, aunque
hay infinitas gemas en el guantelete, a mi sólo me interesa tomar las primeras tres y utilizar esas,
por lo que se descartan las demas y solo se aplican las primeras 3.-}