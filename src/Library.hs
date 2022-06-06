module Library where
import PdePreludat

--Punto 1

--type Guantelete = (String,[String] )

data Guantelete = UnGuantelete {
    material :: String,
    gemas:: [Gema]
} deriving (Show, Eq)

data Personaje = UnPersonaje{
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
    | otherwise =  (debilitarEnergia personaje (energia personaje)){habilidades = []}
    
elTiempo :: Gema
elTiempo personaje = (debilitarEnergia personaje 20){edad = max 18 (div (edad personaje) 2)}

laGemaLoca :: Gema -> Gema
laGemaLoca gema = (gema.gema) 

personaje1 = UnPersonaje {edad=18,energia=22,habilidades=["dormir"],nombre ="Yo", planeta ="Luna"}

--Punto 4
guanteleteGoma = UnGuantelete {material="Goma", gemas=[elTiempo, elAlma "usar Mjolnir", laGemaLoca (elAlma "programacion en Haskell")]}

-- Punto 5
utilizar :: [Gema] -> Personaje -> Personaje
utilizar gemas enemigo = foldl (\enemigo gema -> gema enemigo) enemigo gemas