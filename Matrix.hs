module Matrix where

data Matrix a = M [[a]]

instance Show a => Show (Matrix a) where
  show m = show' m
    where
      show' (M [])     = ""
      show' (M (l:ls)) = show l ++ "\n" ++ show' (M ls)

instance Eq a => Eq (Matrix a) where
  (==) :: Matrix a -> Matrix a -> Bool
  (==) (M a) (M b) = if a == b then True else False

instance Num y => Num (Matrix y) where
  (+) (M (a:as)) (M (b:bs)) = M ((h (+) a b) : (h' (+) as bs)) -- only works if both Matrices have the same dimension!
    where
      h  _ [] []         = []
      h  f (x:xs) (z:zs) = (x `f` z) : (h f xs zs)
      h' _ [] []         = []
      h' f (x:xs) (z:zs) = (h f x z) : (h' f xs zs)
  (-) (M (a:as)) (M (b:bs)) = M ((h (-) a b) : (h' (-) as bs)) -- only works if both Matrices have the same dimension!
    where
      h  _ [] []         = []
      h  f (x:xs) (z:zs) = (x `f` z) : (h f xs zs)
      h' _ [] []         = []
      h' f (x:xs) (z:zs) = (h f x z) : (h' f xs zs)
  

fn :: (a -> a) -> Int -> a -> a
fn f n x = if n == 0 then x else f (fn f (n-1) x)

-- fills empty spaces with 0s (might need refining for neutral element of every Num a instead of 0. z.B. frac "0/1" for my Brüche)
fillMatrix :: Num a => Matrix a -> Matrix a
fillMatrix (M ls) = M $ fill ls
  where
    fill :: Num a => [[a]] -> [[a]]
    fill m = fill' m (biggest m)
      where
        biggest :: Num a => [[a]] -> Int
        biggest [] = 0
        biggest ls = maximum (map length ls)
        fill' :: Num a => [[a]] -> Int -> [[a]]
        fill' [] _     = []
        fill' (l:ls) j = (fn (\z -> z ++ [0]) (j - (length l)) l) : (fill' ls j)

-- dimension of a Matrix
dimension :: Num a => Matrix a -> (Int, Int)
dimension m = dimension' $ fillMatrix m
  where
    dimension' (M (l:ls)) = (length (l:ls), length l)

-- checks if both Matrices have the same dimension
isSameDimension :: Num a => Matrix a -> Matrix a -> Bool
isSameDimension m1 m2 = dimension m1 == dimension m2