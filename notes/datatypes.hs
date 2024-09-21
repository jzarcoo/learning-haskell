-- Las clases de tipos son una especie de interfaz que define algún tipo de comportamiento.
-- Eq, Ord, ...

-- Constructor de tipos "=" Contructor de dato

data Maybe a = Nothing | Just a

-- -----------------------------------------------------------------------------------------------------------------
-- Sintaxis de registro
-- El orden puede variar
-- Car {company = "Ford", model = "Mustang", year = 1967} 
data Car = Car {
  company :: String, 
  model :: String, 
  year :: Int
  } deriving (Eq, Show, Read)
  -- Hs comprueba campo a campo, los cuales deben ser miembros derivados también.

-- -----------------------------------------------------------------------------------------------------------------
-- Datatype with parameters
data Vector a = Vector a a a deriving (Show)

-- Suma 2 vectores
vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

-- Producto vector y escalar
vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

-- Producto escalar
scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

-- -----------------------------------------------------------------------------------------------------------------
-- Especie de enumeraciones
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum) -- Orden por como los escribimos

-- -----------------------------------------------------------------------------------------------------------------
-- Sinónimos de tipo
-- Transmitir algo más de información acerca del cometido de las cadenas en sus funciones y que representan.
type String = [Char]
-- Pueden ser parametrizados
type AssocList k v = [(k,v)]
-- Podemos aplicar parcialmente los parámetros de tipo
type IntMap = Map Int --type IntMap v = Map Int v

-- -----------------------------------------------------------------------------------------------------------------
-- ¿Cómo o por qué está fallando algo?
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

-- Ejemplo (import qualified Data.Map as Map)
data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

-- -----------------------------------------------------------------------------------------------------------------
-- Estructuras de datos recursivas

-- -----------------------------------------------------------------------------------------------------------------
-- Clases de tipos paso a paso (2ª parte)

-- -----------------------------------------------------------------------------------------------------------------
-- La clase de tipos Yes-No

-- -----------------------------------------------------------------------------------------------------------------
-- La clase de tipos funtor

-- -----------------------------------------------------------------------------------------------------------------
-- Familias y artes marciales
