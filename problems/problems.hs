egyptianMultiplication :: (Integral a) => a -> a -> a
egyptianMultiplication a b = aux a b 0

aux :: (Integral a) => a -> a -> a -> a
aux a b acc
  | a <= 0 = acc
  | even a = aux (a `div` 2) (b * 2) acc
  | otherwise = aux (a `div` 2) (b * 2) (acc + b)