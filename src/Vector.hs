{-# LANGUAGE GADTs #-}

module Vector
  ( Vectorizable(..)
  , Vector
  , Value
  , Index
  , Scalar
  ) where

import           Prelude

-- |
-- * Vectors have to implement every important `Vector` operation
--
class Vectorizable a where
  add :: a -> a -> a
  scale :: Scalar -> a -> a
  dotProduct :: a -> a -> Scalar
  crossProduct :: a -> a -> a
  magnitude :: CoordinateSystem -> a -> Angle -> Scalar
  direction :: a -> Angle

newtype Vector =
  Vector [Value]
  deriving (Show)

instance Vectorizable Vector where
  add (Vector values0) (Vector values1) =
    Vector $ zipWith (\a b -> a + b) values0 values1
  scale scalar (Vector values) =
    Vector $ map (\(Value idx scalar') -> Value idx (scalar * scalar')) values
  dotProduct (Vector values0) (Vector values1) =
    sum $
    zipWith
      (\(Value _ value0) (Value _ value1) -> value0 * value1)
      values0
      values1
  crossProduct (Vector [(Value idx0 x0), (Value idx1 y0), (Value idx2 z0)]) (Vector [(Value _ x1), (Value _ y1), (Value _ z1)]) =
    Vector
      [ Value idx0 (y0 * z1 - z0 * x1)
      , Value idx1 (z0 * x1 - x0 * z1)
      , Value idx2 (x0 * y1 - y0 * x1)
      ]
  crossProduct _ _ = Vector []
  cartesianMagnitude (Vector [x, y]) = Prelude.sqrt $ square x + square y
  magnitude (Vector [x, y]) (Angle angle) =
    case [x, y] of
      [0, y'] -> Scalar $ y' * angle
      [x', 0] -> Scalar $ x' * angle
      [x, y]  -> Scalar . Prelude.sqrt $ square x + square y
  direction (Vector [x, y]) = y / x

-- |`Vector Addition` is just the elementwise addition of two equally
-- dimensioned vectors v, an v' at equivalent indeces, such that a
-- new vector of that same dimensions of results from it.
pythagoras :: [Int] -> Int
pythagoras [ak, gk] = Prelude.sqrt $ square ak + square gk
pythagoras _        = 0

square :: Int -> Int
square a = a * a

data Value =
  Value Index Scalar
  deriving (Show, Eq)

instance Num Value where
  (+) (Value idx (Scalar val0)) (Value idx' (Scalar val1)) =
    if idx == idx'
      then Value idx $ Scalar (val0 + val1)
      else Value (Index 0) (Scalar 0)
  (*) (Value idx (Scalar val0)) (Value idx' (Scalar val1)) =
    if idx == idx'
      then Value idx $ Scalar (val0 * val1)
      else Value (Index 0) (Scalar 0)
  abs (Value idx (Scalar val0)) = Value idx $ Scalar (Prelude.abs val0)
  signum (Value idx (Scalar val)) =
    if val > 0
      then Value idx (Scalar 1)
      else if val < 0
             then Value idx (Scalar $ Prelude.negate 1)
             else Value idx (Scalar 0)
  fromInteger integer = Value (Index 0) (Scalar $ Prelude.fromInteger integer)
  negate (Value idx (Scalar val)) = Value idx $ Scalar (Prelude.negate val)

newtype Index =
  Index Int
  deriving (Show, Eq)

newtype Scalar =
  Scalar Int
  deriving (Show, Eq)

instance Num Scalar where
  (+) (Scalar val0) (Scalar val1) = Scalar (val0 + val1)
  (*) (Scalar val0) (Scalar val1) = Scalar (val0 * val1)
  abs (Scalar val) = Scalar $ Prelude.abs val
  signum (Scalar val) =
    if val > 0
      then 1
      else if val < 0
             then -1
             else 0
  fromInteger integer = Scalar $ Prelude.fromInteger integer
  negate (Scalar val) = Scalar $ Prelude.negate val

newtype Angle =
  Angle Int
  deriving (Show, Eq)

instance Num Angle where
  (+) (Angle val0) (Angle val1) = Angle (val0 + val1)
  (*) (Angle val0) (Angle val1) = Angle (val0 * val1)
  abs (Angle val) = Angle $ Prelude.abs val
  signum (Angle val) =
    if val > 0
      then 1
      else if val < 0
             then -1
             else 0
  fromInteger integer = Angle $ Prelude.fromInteger integer
  negate (Angle val) = Angle $ Prelude.negate val
