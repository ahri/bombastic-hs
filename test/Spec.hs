import Test.Hspec
import Data.List
import Bombastic

main :: IO ()
main = hspec $ do
    describe "Map load" $ do
        it "valid map loads correctly" $ do
            opaque `shouldBe` expected
                where
                    opaque = show . opaqueState <$> state
                    state = startGame players <$> mapFromDebug validMap

                    players = [mkPlayer "p1", mkPlayer "p2"]
                    validMap =
                        [ "######"
                        , "#S.  #"
                        , "#.S..#"
                        , "#.. S#"
                        , "######"
                        ]
                    expected = Just . intercalate "\n" $
                        [ "######"
                        , "#0.  #"
                        , "#.1..#"
                        , "#..  #"
                        , "######"
                        ]
