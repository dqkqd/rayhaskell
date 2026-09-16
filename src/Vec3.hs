module Vec3 (
  Vec3 (V3),
  lengthV,
  (*^),
  (^*),
  (^/),
  dot,
  cross,
  unit,
) where

data Vec3 a = V3 a a a deriving (Functor, Eq)

instance Applicative Vec3 where
  pure v = V3 v v v
  (V3 f1 f2 f3) <*> (V3 x y z) = V3 (f1 x) (f2 y) (f3 z)

instance (Num a) => Num (Vec3 a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  fromInteger = pure . fromInteger
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum

instance (Fractional a) => Fractional (Vec3 a) where
  fromRational = pure . fromRational
  (/) = liftA2 (/)

-- mutiply with scalar
(*^) :: (Num a) => a -> Vec3 a -> Vec3 a
(*^) s = fmap (* s)

-- mutiply with scalar
(^*) :: (Num a) => Vec3 a -> a -> Vec3 a
(^*) = flip (*^)

-- devide by scalar
(^/) :: (Fractional a) => Vec3 a -> a -> Vec3 a
(^/) v s = fmap (/ s) v

dot :: (Num a) => Vec3 a -> Vec3 a -> a
dot v1 v2 = x + y + z
 where
  V3 x y z = v1 * v2

cross :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
cross (V3 a1 a2 a3) (V3 b1 b2 b3) =
  V3
    (a2 * b3 - a3 * b2)
    (a3 * b1 - a1 * b3)
    (a1 * b2 - a2 * b1)

lengthV :: (Floating a) => Vec3 a -> a
lengthV v = sqrt (v `dot` v)

unit :: (Floating a) => Vec3 a -> Vec3 a
unit v = v ^/ lengthV v
