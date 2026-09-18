module Interval (Interval (Interval), defaultInterval, inInterval) where

data Interval
  = Interval
      Double -- min
      Double -- max

defaultInterval :: Interval
defaultInterval = Interval 0 (1 / 0)

-- | Whether an interval contains value
--
-- >>> inInterval (Interval 1 10) 5
-- True
--
-- >>> inInterval (Interval 1 10) 12
-- False
--
-- >>> inInterval (Interval 1 10) 0
-- False
inInterval :: Interval -> Double -> Bool
inInterval (Interval minV maxV) value = value >= minV && value <= maxV
