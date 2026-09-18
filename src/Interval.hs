module Interval (
  Interval (Interval),
  defaultInterval,
  intervalContains,
  intervalSurrounds,
) where

data Interval
  = Interval
      Double -- min
      Double -- max

defaultInterval :: Interval
defaultInterval = Interval 0 (1 / 0)

-- | Whether an interval contains a value
-- Return true if x in [a, b]
--
-- >>> intervalContains (Interval 1 10) 5
-- True
--
-- >>> intervalContains (Interval 1 10) 1
-- True
--
-- >>> intervalContains (Interval 1 10) 10
-- True
--
-- >>> intervalContains (Interval 1 10) 12
-- False
--
-- >>> intervalContains (Interval 1 10) 0
-- False
intervalContains :: Interval -> Double -> Bool
intervalContains (Interval minV maxV) value = minV <= value && value <= maxV

-- | Where an interval surrounds a value
-- Return true if x in (a, b)
--
-- >>> intervalSurrounds (Interval 1 10) 5
-- True
--
-- >>> intervalSurrounds (Interval 1 10) 1
-- False
--
-- >>> intervalSurrounds (Interval 1 10) 10
-- False
intervalSurrounds :: Interval -> Double -> Bool
intervalSurrounds (Interval minV maxV) value = minV < value && value < maxV
