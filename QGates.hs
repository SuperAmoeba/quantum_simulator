module QGates where

import Matrix
import Complex

type State a = Matrix a
type Gate a = (State a -> State a)


-- Pauli-X
x :: Num a => Gate a
x = \s -> (M [[0,1],[1,0]]) * s

-- Pauli-Y
y :: (Num a, RealFloat a) => Gate (Complex a)
y = \s -> (M [[0, 0 :+ (-1)],[0 :+ 1, 0]]) * s

-- Pauli-Z
z :: Num a => Gate a
z = \s -> (M [[1,0],[0,-1]]) * s

-- Phase
s :: (Num a, RealFloat a) => Gate (Complex a)
s = \s' -> (M [[1,0],[0, 0 :+ 1]]) * s'

-- Hadamard needs Fractions...