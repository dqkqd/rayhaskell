module Vec3 (Vec3, lengthV, fromTuple, toTuple, dot, cross, unit) where

data Vec3' a = Vec3' a a a deriving (Functor, Eq)

fromTuple :: (a, a, a) -> Vec3' a
fromTuple (x, y, z) = Vec3' x y z

toTuple :: Vec3' a -> (a, a, a)
toTuple (Vec3' x y z) = (x, y, z)

instance Applicative Vec3' where
  pure v = Vec3' v v v
  (Vec3' f1 f2 f3) <*> (Vec3' x y z) = Vec3' (f1 x) (f2 y) (f3 z)

-- Vec3 implementation
type Vec3 = Vec3' Double

lengthV :: Vec3 -> Double
lengthV (Vec3' x y z) = sqrt (x * x + y * y + z * z)

instance Num Vec3 where
  (+) = liftA2 (+)
  (*) = liftA2 (*)

  fromInteger = pure . fromInteger

  negate = fmap negate

  -- vector has no abs
  abs = undefined

  -- vector has no sign
  signum = undefined

instance Fractional Vec3 where
  fromRational = pure . fromRational
  (/) = liftA2 (/)

dot :: Vec3 -> Vec3 -> Double
dot (Vec3' a1 a2 a3) (Vec3' b1 b2 b3) = a1 * b1 + a2 * b2 + a3 * b3

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3' a1 a2 a3) (Vec3' b1 b2 b3) =
  Vec3'
    (a2 * b3 - a3 * b2)
    (a3 * b1 - a1 * b3)
    (a1 * b2 - a2 * b1)

unit :: Vec3 -> Vec3
unit v = v / fromRational (toRational $ lengthV v)
