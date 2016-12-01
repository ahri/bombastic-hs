import Test.Hspec
import Data.List
import Bombastic

validMap :: [String]
validMap =
    [ "######"
    , "#S.  #"
    , "#.S..#"
    , "#.. S#"
    , "######"
    ]

main :: IO ()
main = hspec $ do
    describe "Map load" $ do
        it "valid map loads correctly" $ do
            opaque `shouldBe` expected
                where
                    players = [mkPlayer "p1", mkPlayer "p2"]
                    state = startGame players <$> mapFromDebug validMap
                    opaque = show . opaqueState <$> state
                    expected = Just (intercalate "\n"
                        [ "######"
                        , "#0.  #"
                        , "#.1..#"
                        , "#..  #"
                        , "######"
                        ])
