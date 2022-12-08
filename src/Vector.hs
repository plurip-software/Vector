{-# LANGUAGE GADTs #-}

module Vector (Vectorizable, Vector, Value, Index, Scalar) where

import Prelude

class Vectorizable a where
    add          :: a -> a -> a
    scale        :: Scalar -> a -> a
    dotProduct   :: a -> a -> Int
    crossProduct :: a -> a -> a
    magnitude    :: a -> Scalar
    direction    :: a -> Angle

newtype Vector = Vector [Value] deriving Show

instance Vectorizable Vector where

    {-|
    `Vector Addition` is just the elementwise addition of two equally
    dimensioned vectors v, an v' at equivalent indeces, such that a 
    new vector of that same dimensions of results from it
    -}
    add (Vector values0) (Vector values1) =
        Vector $ zipWith (\a b -> a + b) values0 values1

    {-|
    A so called `Scalar` is some value that only has a magnitude instead
    of beeing like a vector consisting of more dimensions than that.

    As the word already suggests, a `Scalar`, scales a `Vector` v by some
    `Scalar` s such that the whole `Vector`'s magnitude becomes larger by the
    size of that `Scalar`, resulting in an elongated or scaled, result `Vector`
    v' with equivalent dimensions as the starting `Vector` v.   
    -}
    scale scalar (Vector values) = 
        Vector $ map (\(Value idx scalar') -> Value idx (scalar * scalar')) values

    {-|
    The Dot-Product is the sum s of the products val0 and val1 of two vectors
    such that the values at equivalent indeces are multiplied as follows...

    ```
    (x0, y0, z0) * (x1, y1, z1) = x0 * x1 + y0 * y1 + z0 * z1
    ```
    -}
    dotProduct (Vector values0) (Vector values1) =
        sum $ zipWith (\(Value _ (Scalar value0)) (Value _ (Scalar value1)) -> value0 * value1) values0 values1

    -- crossProduct :: a -> a -> Int
    -- | If the two vectors, have the same or the exact opposite direction,
    -- or if one of the vectors, has a length of 0,
    -- then the product equals 0.
    --
    -- The Cross-Product is only defined in 3D and is computed as follows...
    --
    -- ```
    -- x0, y0, z0) * (x1, y1, z1) = (y0z1 - z0y1, z0x1 - x0z1, x0y1 - y0x1)
    -- OR
    -- a × b = |a| |b| sin(θ) n
    -- ```
    --
    crossProduct (Vector [(Value idx0 x0), (Value idx1 y0), (Value idx2 z0)]) (Vector [(Value _ x1), (Value _ y1), (Value _ z1)]) =
        Vector 
            [ Value idx0 (y0 * z1 - z0 * x1)
            , Value idx1 (z0 * x1 - x0 * z1)
            , Value idx2 (x0 * y1 - y0 * x1)
            ]
    crossProduct _ _ = Vector []

    -- magnitude    :: a -> Scalar
    -- direction    :: a -> Angle

data Value 
    = Value Index Scalar deriving (Show, Eq)

instance Num Value where
    (+) (Value idx (Scalar val0)) (Value idx' (Scalar val1)) = 
        if idx == idx'
        then Value idx $ Scalar (val0 + val1)
        else Value (Index 0) (Scalar 0)
    (*) (Value idx (Scalar val0)) (Value idx' (Scalar val1)) = 
        if idx == idx'
        then Value idx $ Scalar (val0 * val1)
        else Value (Index 0)(Scalar 0)
    abs (Value idx (Scalar val0)) = Value idx $ Scalar (Prelude.abs val0)
    signum (Value idx (Scalar val)) = 
        if val > 0 
        then Value idx (Scalar 1) 
        else if val < 0 
        then Value idx (Scalar $ Prelude.negate 1 )
        else Value idx (Scalar 0)
    fromInteger integer = Value (Index 0) (Scalar $ Prelude.fromInteger integer)
    negate (Value idx (Scalar val)) = Value idx $ Scalar (Prelude.negate val)

newtype Index = Index Int deriving (Show, Eq)

newtype Scalar = Scalar Int deriving (Show, Eq)

instance Num Scalar where
    (+) (Scalar val0) (Scalar val1) = Scalar (val0 + val1)
    (*) (Scalar val0) (Scalar val1) = Scalar (val0 * val1)
    abs (Scalar val) = Scalar $ Prelude.abs val
    signum (Scalar val) = if val > 0 then 1 else if val < 0 then -1 else 0 
    fromInteger integer = Scalar $ Prelude.fromInteger integer
    negate (Scalar val) = Scalar $ Prelude.negate val

newtype Angle = Angle Int deriving (Show, Eq)

instance Num Angle where
    (+) (Angle val0) (Angle val1) = Angle (val0 + val1)
    (*) (Angle val0) (Angle val1) = Angle (val0 * val1)
    abs (Angle val) = Angle $ Prelude.abs val
    signum (Angle val) = if val > 0 then 1 else if val < 0 then -1 else 0 
    fromInteger integer = Angle $ Prelude.fromInteger integer
    negate (Angle val) = Angle $ Prelude.negate val