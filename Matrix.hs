module Matrix where

import Complex

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
  (+) (M (a:as)) (M (b:bs)) = if not (isSameDimension (M (a:as)) (M (b:bs))) then error "Matrices don't have the same dimension (+)"
                              else
                                M ((h (+) a b) : (h' (+) as bs))
                                where
                                  h  _ [] []         = []
                                  h  f (x:xs) (z:zs) = (x `f` z) : (h f xs zs)
                                  h' _ [] []         = []
                                  h' f (x:xs) (z:zs) = (h f x z) : (h' f xs zs)
  (-) (M (a:as)) (M (b:bs)) = if not (isSameDimension (M (a:as)) (M (b:bs))) then error "Matrices don't have the same dimension (-)"
                              else
                                M ((h (-) a b) : (h' (-) as bs)) 
                                where
                                  h  _ [] []         = []
                                  h  f (x:xs) (z:zs) = (x `f` z) : (h f xs zs)
                                  h' _ [] []         = []
                                  h' f (x:xs) (z:zs) = (h f x z) : (h' f xs zs)
  (*) (M m1) m2 = if not (isMultipliable (M m1) m2) then error "Matrices not multipliable"
              else let (M m2_t) = transpose m2 in M (h m1 m2_t)
                where
                  h :: Num y => [[y]] -> [[y]] -> [[y]]
                  h [] _ = []
                  h (a:as) bs = (h' a bs) : (h as bs)
                  h' :: Num y => [y] -> [[y]] -> [y]
                  h' _ [] = []
                  h' a (b:bs) = (foldl (+) 0 (zipWith (*) a b)) : (h' a bs)

class Conjugatable a where
  conj :: a -> a

instance RealFloat a => Conjugatable (Complex a) where
  conj = conjugate

instance Conjugatable Int where
  conj = id
instance Conjugatable Integer where
  conj = id
instance Conjugatable Double where
  conj = id
instance Conjugatable Float  where
  conj = id


-- applies a function f n times
fn :: (a -> a) -> Int -> a -> a
fn f n x = if n == 0 then x else f (fn f (n-1) x)

-- removes the i-th element of a list
(/!) :: [a] -> Int -> [a]
(/!) xs i = h xs i 0
  where
    h :: [a] -> Int -> Int -> [a]
    h [] _ _ = []
    h (x:xs) i j = if i == j then xs else x : h xs i (j+1)

-- map on [[a]]
mapLL :: (a -> a) -> [[a]] -> [[a]]
mapLL f [] = []
mapLL f (l:ls) = (map f l) : (mapLL f ls)

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

-- for input of a Matrix
mkMatrix :: Num a => [[a]] -> Matrix a
mkMatrix m = fillMatrix $ M m

-- dimension of a Matrix
dimension :: Num a => Matrix a -> (Int, Int)
dimension m = dimension' $ fillMatrix m
  where
    dimension' (M (l:ls)) = (length (l:ls), length l)

-- checks if both Matrices have the same dimension
isSameDimension :: Num a => Matrix a -> Matrix a -> Bool
isSameDimension m1 m2 = dimension m1 == dimension m2

-- checks if the Matrices are multipliable
isMultipliable :: Num a => Matrix a -> Matrix a -> Bool
isMultipliable m1 m2 =
  let
    (_, d1s) = dimension m1
    (d2z, _) = dimension m2
  in
    d1s == d2z

-- A^T  for any Matrix A
transpose :: Num y => Matrix y -> Matrix y
transpose m = let (z, s) = dimension m in M (h m (s-1))
  where
    h (M as) (-1) = []
    h (M as) n = h (M as) (n-1) ++ [map (!! n) as]

-- conjugate transpose, duh
conjugateTranspose :: (Num y, Conjugatable y) => Matrix y -> Matrix y
conjugateTranspose (M a) = transpose $ M (mapLL conj a)

-- checks if the Matrix is a diagonal Matrix
isDiagonal :: (Num y, Eq y) => Matrix y -> Bool
isDiagonal a = let (m, n) = dimension a in
  if m /= n then False
  else foldl (+) 0 (h a 0) == 0
    where
      h :: Num y => Matrix y -> Int -> [y]
      h (M []) _ = []
      h (M (a:as)) i = (a /! i) ++ h (M as) (i+1)

-- tensor product
(*/) :: Num y => Matrix y -> Matrix y -> Matrix y
(*/) (M a) (M b) = M (h a b b)
  where
    h :: Num y => [[y]] -> [[y]] -> [[y]] -> [[y]]
    h [] _ _           = []
    h (a:as) [] b'     = h as b' b'
    h (a:as) (b:bs) b' = (g a b) : (h (a:as) bs b')
      where
        g :: Num y => [y] -> [y] -> [y]
        g [] _ = []
        g (x:xs) zs = map (x *) zs ++ (g xs zs)