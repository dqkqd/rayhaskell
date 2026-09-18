module Vec3 (
  Vec3 (V3),
  mkVec3,
  lengthSquare,
  (*^),
  (^*),
  (^/),
  dot,
  cross,
  unit,
) where

-- | Generic vector
data Vec3 a = V3 a a a deriving (Functor, Foldable, Eq, Show)

mkVec3 :: (Vec3 b -> a) -> b -> b -> b -> a
mkVec3 wrap x y z = wrap (V3 x y z)

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

-- | Mutiply a scalar
--
-- >>> 5 *^ (V3 1 2 3)
-- V3 5 10 15
(*^) :: (Functor f, Num a) => a -> f a -> f a
(*^) s = fmap (* s)

-- | Mutiply a scalar
--
-- >>> (V3 1 2 3) ^* 5
-- V3 5 10 15
(^*) :: (Functor f, Num a) => f a -> a -> f a
(^*) = flip (*^)

-- | Divide by scalar
--
-- >>> (V3 5 10 15) ^/ 5
-- V3 1.0 2.0 3.0
(^/) :: (Functor f, Fractional a) => f a -> a -> f a
(^/) v s = fmap (/ s) v

-- | Dot product
--
-- >>> (V3 1 2 3) `dot` (V3 4 5 6)
-- 32
dot :: (Applicative f, Foldable f, Num a) => f a -> f a -> a
dot v1 v2 = sum $ (*) <$> v1 <*> v2

-- | Cross product between two Vec3
--
-- >>> (V3 1 2 3) `cross` (V3 4 5 6)
-- V3 (-3) 6 (-3)
cross :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
cross (V3 a1 a2 a3) (V3 b1 b2 b3) =
  V3
    (a2 * b3 - a3 * b2)
    (a3 * b1 - a1 * b3)
    (a1 * b2 - a2 * b1)

-- | Length of a Vec3
--
-- >>> lengthSquare (V3 2 3 6)
-- 49.0
lengthSquare :: (Floating a) => Vec3 a -> a
lengthSquare v = v `dot` v

-- | Unit vector for Vec3
--
-- >>> unit (V3 1 2 2)
-- V3 0.3333333333333333 0.6666666666666666 0.6666666666666666
unit :: (Floating a) => Vec3 a -> Vec3 a
unit v = v ^/ (sqrt . lengthSquare) v
