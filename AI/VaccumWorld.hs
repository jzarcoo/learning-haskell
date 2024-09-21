module VaccumWorld (
  newEnviroment,
) where

import Cleanable 
    ( Cleanable(clean) )
import Square
    ( Square, newSquare, setDirt, setLeft, setRight, setUp, setDown )

import System.Random (randomIO)
import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)

data VaccumWorld = VaccumWorld {
  dirtInit :: String,
  squares :: [Square]
  }

instance Show VaccumWorld where
  show :: VaccumWorld -> String
  show (VaccumWorld _ squares) = "[" ++ show squares ++ "]"

newEnviroment :: String -> VaccumWorld
newEnviroment dirtInit = VaccumWorld dirtInit squares
  where
    a = newSquare "A"
    b = newSquare "B"
    c = newSquare "C"
    d = newSquare "D"
    a' = setRight a b
    a'' = setDown a c
    b' = setLeft b a
    b'' = setDown b d
    c' = setUp c a
    c'' = setRight c d
    d' = setUp d b
    d'' = setLeft d c
    squares = initializeDirt [a'', b'', c'', d''] dirtInit

initializeDirt :: [Square] -> String -> [Square]
initializeDirt squares "random" = map (\square -> if getRandom then setDirt square 1 else square) squares
initializeDirt squares "dirty" = map (`setDirt`1) squares
initializeDirt squares "clean" = map clean squares

getRandom :: IO Bool
getRandom = do
    n <- randomRIO (50, 100) 
    return $ even n 