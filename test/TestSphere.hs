module TestSphere (spec) where

import Hit (Hittable (hit), defaultInterval)
import Point (point)
import Ray (Ray (Ray))
import Sphere (Sphere (Sphere))
import Test.Hspec (Spec, describe, it, shouldBe)
import Vec3 (Vec3 (V3))

spec :: Spec
spec = do
  describe "Hittable" $ do
    let sphere = Sphere (point 2 2 0) 2

    it "No intersection" $ do
      let ray = Ray (point 5 (-1) 0) (V3 0 1 0)
      show (hit sphere ray defaultInterval) `shouldBe` "Nothing"

    it "One intersection" $ do
      let ray = Ray (point 4 (-1) 0) (V3 0 1 0)
      show (hit sphere ray defaultInterval)
        `shouldBe` "Just (HitRecord (P (V3 4.0 2.0 0.0)) (V3 1.0 0.0 0.0))"

    it "Two intersection" $ do
      let ray = Ray (point 2 (-1) 0) (V3 0 1 0)
      show (hit sphere ray defaultInterval)
        `shouldBe` "Just (HitRecord (P (V3 2.0 0.0 0.0)) (V3 0.0 (-1.0) 0.0))"
