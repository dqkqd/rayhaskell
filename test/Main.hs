import Test.Hspec (hspec)

import TestSphere qualified

main :: IO ()
main = hspec $ do
  TestSphere.spec
