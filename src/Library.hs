module Library where
import PdePreludat

doble n= n*2

factorial 0 = 1
factorial n | n>0 = n * factorial (n-1)

estaOrdenada [] = True
estaOrdenada [x] = True
estaOrdenada (primero:(segundo:cola)) = primero <= segundo && estaOrdenada (segundo:cola)


any' _ [] = False
any' condicion (cabeza:cola) = condicion cabeza || any' condicion cola

map' transformacion [] = []
map' transformacion (x:xs) = (transformacion x):(map' transformacion xs)


last' [x] = x
last' (x:xs) = last' xs


-- repeat' e = e:(repeat' e)\


data Persona = UnaPersona { nombre :: String, peso :: Number} deriving (Show)
data Rosquilla = UnaRosquilla { gramos :: Number } deriving (Show)

homero = UnaPersona "homero" 64
herbert = UnaPersona "herbert" 80

rosquillaGrande = UnaRosquilla 150
rosquillaChica = UnaRosquilla 50

comerUna :: Persona -> Rosquilla -> Persona
comerUna persona rosquilla = 
    persona {peso = peso persona * 0.99 
    + gramos rosquilla}

comerMuchas :: Persona -> [Rosquilla] -> Persona
comerMuchas persona rosquillas = foldl comerUna persona rosquillas

sum' lista = foldl (+) 0 lista

sum'' lista = foldl1 (+) lista

sum''' lista = foldl (\n1 n2 -> n1 + n2)  0 lista



personaConMayorNombre:: [Persona] -> Persona
personaConMayorNombre personas = foldl1 quedarseConMayor personas

quedarseConMayor :: Persona -> Persona -> Persona
quedarseConMayor p1 p2 
    | longitudNombre p1 > longitudNombre p2 = p1
    | otherwise = p2

longitudNombre = length.nombre


