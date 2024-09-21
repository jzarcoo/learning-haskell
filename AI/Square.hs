module Square (
  Square,
  newSquare,
  setDirt,
  setLeft,
  setRight,
  setUp,
  setDown,
) where

import Cleanable ( Cleanable(..) )

type Nombre = String

data Square = Square {
  name :: Nombre,
  dirt :: Int,
  left :: Maybe Square,
  right :: Maybe Square,
  up :: Maybe Square,
  down :: Maybe Square
  }

instance Show Square where
  show :: Square -> String
  show (Square name dirt _ _ _ _) = "(" ++ name ++ ", " ++ show dirt ++ ")"

instance Cleanable Square where
  clean :: Square -> Square
  clean = (`setDirt` 0)
  
newSquare :: Nombre -> Square
newSquare n = square
  where
    square = Square n 0 (Just square) (Just square) (Just square) (Just square)

setDirt :: Square -> Int -> Square
setDirt square n = square {dirt = n}

setLeft :: Square -> Square -> Square
setLeft square1 square2 = square1 {left = Just square2}

setRight :: Square -> Square -> Square
setRight square1 square2 = square1 {right = Just square2}

setUp :: Square -> Square -> Square
setUp square1 square2 = square1 {up = Just square2}

setDown :: Square -> Square -> Square
setDown square1 square2 = square1 {down = Just square2}
