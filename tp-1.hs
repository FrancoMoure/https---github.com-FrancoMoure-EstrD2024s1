--2.1)
sucesor :: Int -> Int
sucesor x = x + 1

sumar :: Int -> Int 
sumar x y = x + y

divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto x y = (a `div` b, a `mod` b)

maxDelPar :: (Int, Int) -> Int 
maxDelPar (x, y) = if x>y then x else y

--2.2)
--1)
maxDelPar(divisionYResto(sumar 5 5)(sucesor 0))

--2)
maxDelPar (divisionYResto (sumar 5 5) 1)

--3)
sucesor (sucesor (sucesor (sucesor (sucesor 5))))

--4)
sumar 5 (maxDelPar (divisionYResto 20 4))

--3.1)
data Dir = Norte | Sur | Este | Oeste 
    deriving Show

opuesto :: Dir -> Dir
opuesto Norte = Sur 
opuesto Sur = Norte 
opuesto Este = Oeste 
opuesto Oeste = Este 

iguales :: Dir -> Dir -> Bool 
iguales Norte Norte = True 
iguales Sur Sur = True 
iguales Oeste Oeste = True
iguales Este Este = True 
iguales _ _ = False 

siguiente :: Dir -> Dir 
siguiente Norte = Este 
siguiente Este = Sur 
siguiente Sur = Oeste 
siguiente Oeste = error"Oeste no tiene direccion siguiente"

--3.2)
data DiaDeLaSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
    deriving Show

primeroYUltimoDia :: (DiaDeLaSemana, DiaDeLaSemana)
primeroYUltimoDia = (Lunes, Domingo)

empiezaConM :: DiaDeLaSemana -> Bool 
empiezaConM Martes = True 
empiezaConM Miercoles = True 
empiezaConM _ = False 

vieneDespues :: DiaDeLaSemana -> DiaDeLaSemana -> Bool 
vieneDespues d1 d2 = numeroSegunDia d1 > numeroSegunDia d2

numeroSegunDia :: DiaDeLaSemana -> 1
numeroSegunDia Lunes = 1
numeroSegunDia Martes = 2
numeroSegunDia Miercoles = 3
numeroSegunDia Jueves = 4
numeroSegunDia Viernes = 5
numeroSegunDia Sabado = 6
numeroSegunDia Domingo = 7

estaEnElMedio :: DiaDeLaSemana -> Bool 
estaEnElMedio Lunes = False 
estaEnElMedio Domingo = False 
estaEnElMedio _ = True 

--3.3)
negar :: Bool -> Bool
negar True = False 
negar False = True 

implica :: Bool -> Bool -> Bool 
implica True b = b 
implica False _ = True

yTambien :: Bool -> Bool -> Bool 
yTambien True b = b 
yTambien False _ = False

oBien :: Bool -> Bool -> Bool 
oBien True _ = True 
oBien _ y = y 

--4.1)

data Persona = P String Int 
    deriving Show 

nombre :: Persona -> String 
nombre (P n e) = n 

edad :: Persona -> Int 
edad (P n e) = e 

crecer :: Persona -> Persona 
crecer (P n e) = P n (e+1)

cambioDeNombre :: String -> Persona -> Persona 
cambioDeNombre nuevoNombre (P _ e) = P nuevoNombre e

esMayorQueLaOtra :: Persona -> Persona -> Bool 
esMayorQueLaOtra p1 p2 = edad p1 > edad p2 

laQueEsMayor :: Persona -> Persona -> Persona 
laQueEsMayor p1 p2 = if esMayorQueLaOtra p1 p2 then p1 else p2 

--4.2)
data TipoDePokemon = Agua | Fuego | Planta 
    deriving Show 

data Pokemon = Pok TipoDePokemon Float 
    deriving Show

superaA :: Pokemon -> Pokemon -> Bool 
superaA pk1 pk2 = esSuperiorA (tipoDelPokemon pk1) (tipoDelPokemon pk2)

esSuperiorA :: TipoDePokemon -> TipoDePokemon -> Bool 
esSuperiorA Agua Fuego = True 
esSuperiorA Fuego Planta = True 
esSuperiorA Planta Agua = True 
esSuperiorA _ _ = False 

tipoDelPokemon :: Pokemon -> TipoDePokemon  
tipoDelPokemon (Pok pk _) = pk 

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int 
cantidadDePokemonDe tp (Ent n pk1 pk2) = unoSiCeroSino(esDelMismoTipo (tp) (tipoDelPokemon pk1)) + unoSiCeroSino(esDelMismoTipo (tp) (tipoDelPokemon pk2))


unoSiCeroSino :: Bool -> Int 
unoSiCeroSino True = 1 
unoSiCeroSino False = 0 

esDelMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esDelMismoTipo Agua Agua = True 
esDelMismoTipo Planta Planta = True 
esDelMismoTipo Fuego Fuego = True 
esDelMismoTipo _ _ = False 

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (ent1, ent2) = pokemonDeEntrenador ent1 ++ pokemonDeEntrenador ent2 

pokemonDeEntrenador :: Entrenador -> [Pokemon]
pokemonDeEntrenador (Ent _ pk1 pk2) = [pk1, pk2]

--5.1)

loMismo :: a -> a 
loMismo x = x 

siempreSiete :: a -> Int 
siempreSiete _ = 7 

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

--5.2)
--dichas funciones son polimorficas ya que pueden ser adaptadas a cualquier tipo algebraico

--6.1)
estaVacia :: [a] -> Bool
estaVacia [] = True 
estaVacia _ = False 

elPrimero :: [a] -> a
--prec: la lista debe tener elementos
elPrimero [] = error"la lista esta vacia"
elPrimero (x:_) = x 

sinElPrimero :: [a] -> [a]
--prec: la lista debe tener elementos
sinElPrimero [] = error"la lista esta vacia"
sinElPrimero (_:xs) = xs 

splitHead :: [a] -> (a, [a])
--prec: la lista debe tener elementos
splitHead [] = error"la lista esta vacia"
splitHead (x:xs) = >(x, xs)





