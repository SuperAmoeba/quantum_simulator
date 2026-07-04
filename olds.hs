import Fraction
import BinaryFraction

-- Helpers :3
invert :: [a] -> [a]
invert []     = []
invert [x]    = [x]
invert (x:xs) = invert xs ++ [x]

-- converts a fraction to a continuous fraction
to_continued_fraction :: Fraction -> [Int]
to_continued_fraction (Bruch (y,1)) = [y]
to_continued_fraction x =
  let f = truncate $ to_double x
  in [f] ++ to_continued_fraction (swap ((kurz x) - fromIntegral f))

-- converts a continuous fraction to a fraction
from_continued_fraction :: [Int] -> Fraction
from_continued_fraction []  = Bruch (0,1)
from_continued_fraction [x] = Bruch (x,1)
from_continued_fraction ls  = swap (h (invert ls))
  where h :: [Int] -> Fraction
        h (x:y:xys) = h1 xys (swap ((Bruch (1,x)) + fromIntegral y))
          where h1 :: [Int] -> Fraction -> Fraction
                h1 [] b = b
                h1 (x:xs) b = h1 xs (swap (b + fromIntegral x))

-- ideia para f^n
fn :: (a -> a) -> Int -> a -> a
fn f n x = if n == 0 then x else f (fn f (n-1) x)