module Fraction where

import Data.Ratio (numerator, denominator)
import Complex


----- FRACTIONS ----- (pls only - in the Zähler)
data Fraction a = Bruch (a, a)

-- how to show fractions
instance Show a => Show (Fraction a) where
  show (Bruch (x,y)) = show x ++ "/" ++ show y

-- eq operations on fractions
instance (Eq a, Num a) => Eq (Fraction a) where
  (==) (Bruch (x1,y1)) (Bruch (x2,y2)) = x1*y2 == x2*y1

-- ord operations on fractions
instance (Num a, Ord a) => Ord (Fraction a) where
  (>) (Bruch (x1,y1)) (Bruch (x2,y2)) = x1*y2 > x2*y1
  (>=) b1 b2                          = b1 > b2 || b1 == b2
  (<) b1 b2                           = not (b1 >= b2)
  (<=) b1 b2                          = b1 < b2 || b1 == b2

-- num (arithmetic) operations on fractions
instance Num a => Num (Fraction a) where
  (+) (Bruch (x1,y1)) (Bruch (x2,y2)) = Bruch (x1*y2 + x2*y1 , y1*y2)
  (-) (Bruch (x1,y1)) (Bruch (x2,y2)) = Bruch (x1*y2 - x2*y1, y1*y2)
  (*) (Bruch (x1,y1)) (Bruch (x2,y2)) = Bruch (x1*x2, y1*y2)
  abs (Bruch (x,y)) = Bruch (abs x, abs y)
  {-signum :: (Eq a, Num a) => Fraction a -> Fraction a
  signum b | b == Bruch (0,1) = Bruch (0,1)
           | b < Bruch (0,1) = Bruch ((-1),1)
           | b > Bruch (0,1) = Bruch (1,1)-}
  fromInteger :: Integer -> Fraction a
  fromInteger x = Bruch (fromInteger x,1)

instance Fractional a => Fractional (Fraction a) where
  (/) b1 b2 = b1 * (recip b2)
  recip (Bruch (x,y)) = Bruch (y,x)
  fromRational :: Rational -> Fraction a
  fromRational x =
    let r = toRational x
    in (Bruch (fromIntegral (numerator r), fromIntegral (denominator r)))

-- some renaming to go easy on my brain :3
swap :: Fractional a => Fraction a -> Fraction a
swap = recip

to_fraction :: Fractional a => Rational -> Fraction a
to_fraction = fromRational

-- for easy input
frac :: String -> Fraction Int
frac b = Bruch $ toIntTuple $ seperate b
  where
    seperate :: String -> (String,String)
    seperate b = (until b, after b)
      where until, after :: String -> String
            until ('/':xs) = ""
            until (x:xs)   = x : until xs
            after ('/':xs) = xs
            after (x:xs)   = after xs
    toIntTuple :: (String,String) -> (Int,Int)
    toIntTuple (x,y) = ((read x :: Int),(read y :: Int))

-- fraction to decimal
to_double :: RealFrac a => Fraction a -> Double
to_double (Bruch (x,y)) = realToFrac x / realToFrac y

-- völlig kurz the fraction
kurz :: Integral a => Fraction a -> Fraction a
kurz (Bruch (x,y)) =
  let g = gcd x y
  in Bruch (div x g, div y g)