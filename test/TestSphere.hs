module TestSphere (spec) where

import Data.Maybe (fromJust)
import Hit (HitRecord (HitRecord), Hittable (hit), defaultInterval)
import Point (point)
import Ray (Ray (Ray))
import Sphere (Sphere (Sphere))
import Test.Hspec (Spec, describe, it, shouldBe)
import Vec3 (Vec3 (V3))

spec :: Spec
spec = do
  describe "Hittable" $ do
    let sphere = Sphere (point 2 2 0) 2

    it "Normal vector pointed outward" $ do
      let ray = Ray (point 4 (-1) 0) (V3 0 1 0)
      let (HitRecord _ normalVec) = fromJust $ hit sphere ray defaultInterval
      normalVec `shouldBe` V3 1 0 0

    it "One intersection" $ do
      let ray = Ray (point 4 (-1) 0) (V3 0 1 0)
      hit sphere ray defaultInterval
        `shouldBe` Just (HitRecord (point 4 2 0) (V3 1 0 0))

    it "Two intersection" $ do
      let ray = Ray (point 2 (-1) 0) (V3 0 1 0)
      hit sphere ray defaultInterval
        `shouldBe` Just (HitRecord (point 2 0 0) (V3 0 (-1) 0))
