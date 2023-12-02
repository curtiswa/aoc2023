import Test.Hspec
import Hedgehog


main = hspec $ do
  describe "unit test" $ do
    it "does something" $ do
      1 `shouldBe` 2
