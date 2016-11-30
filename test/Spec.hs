import Test.Hspec
-- import Bombastic

main :: IO ()
main = hspec $ do
    describe "foo" $ do
        it "does something" $ do
            (1 :: Integer) `shouldBe` (2 :: Integer)
