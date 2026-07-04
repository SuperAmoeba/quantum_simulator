module BinaryFraction where

import Fraction
import Data.Function (on) -- on op f x y = op (f x) (f y)  -- runs an operation on two inputs that are ran first by a function

-- helpers :3
lift2 :: (Fraction -> Fraction -> Fraction) -> BinaryFraction -> BinaryFraction -> BinaryFraction
lift2 f b1 b2 = to_binary_fraction $ f (from_binary_fraction b1) (from_binary_fraction b2)

lift1 :: (Fraction -> Fraction) -> BinaryFraction -> BinaryFraction
lift1 f = to_binary_fraction . f . from_binary_fraction


----- BINARY FRACTIONS ----- (pls only 0 < b < 1)
data BinaryFraction = Zero BinaryFraction | One BinaryFraction | End

-- how to show binary fractions
instance Show BinaryFraction where
  show bf = "0." ++ go bf
    where
      go (Zero r) = "0" ++ go r
      go (One r)  = "1" ++ go r
      go End      = ""

-- use eq operations on fractions for binary fractions
instance Eq BinaryFraction where
  (==) = (==) `on` from_binary_fraction

-- use ord operations on fractions for binary fractions
instance Ord BinaryFraction where
  compare = compare `on` from_binary_fraction

-- use num operations on fractions for binary fractions
instance Num BinaryFraction where
  (+) = lift2 (+)
  (-) = lift2 (-)
  (*) = lift2 (*)
  abs = lift1 abs -- egal, negative binary fractions don't work here
  signum = lift1 signum -- auch egal ^
  fromInteger = to_binary_fraction . fromInteger -- egal, cause b < 1

-- for easy input
binFrac :: String -> BinaryFraction
binFrac (zero:dot:r) = binFrac' r
  where
    binFrac' [] = End
    binFrac' (x:xs) = if x == '0' then Zero $ binFrac' xs else One $ binFrac' xs

-- fraction to binary fraction
to_binary_fraction :: Fraction -> BinaryFraction
to_binary_fraction b = h b 1
  where
    h :: Fraction -> Int -> BinaryFraction
    h (Bruch (0,1)) _ = End
    h b i = 
      let cur = (Bruch (1,2^i))
      in if b >= (Bruch (1,2^i)) then One (h (b - (Bruch (1,2^i))) (i+1)) else Zero (h b (i+1))

-- binary fraction to fraction
from_binary_fraction :: BinaryFraction -> Fraction
from_binary_fraction b = h b 1
  where
    h :: BinaryFraction -> Int -> Fraction
    h End     _  = Bruch (0,1)
    h (One r) i  = Bruch (1,2^i) + (h r (i+1))
    h (Zero r) i = h r (i+1)