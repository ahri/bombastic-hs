import Test.Hspec
import Data.List
import Bombastic

validMap :: [String]
validMap =
    [ "###################"
    , "#S ..       .... S#"
    , "# # # # # # #.#.# #"
    , "#..       # ......#"
    , "# # # # ##### # # #"
    , "#S        #      S#"
    , "###################"
    ]

main :: IO ()
main = hspec $ do
    describe "Map load" $ do
        let
            invalidMap =
                [ "######"
                , "#S. !#"
                , "#.S..#"
                , "#.. S#"
                , "######"
                ]

        it "valid map loads correctly" $ do
            let
                opaque = show . opaqueState <$> state
                state = startGame players <$> mapFromDebug validMap
                players = [mkPlayer "p1", mkPlayer "p2"]

                expected = Just . intercalate "\n" $
                    [ "###################"
                    , "#0 ..       .... 1#"
                    , "# # # # # # #.#.# #"
                    , "#..       # ......#"
                    , "# # # # ##### # # #"
                    , "#         #       #"
                    , "###################"
                    ]

            opaque `shouldBe` expected

        it "invalid map loads as Nothing" $ do
            let
                opaque = show . opaqueState <$> state
                state = startGame players <$> mapFromDebug invalidMap
                players = [mkPlayer "p1", mkPlayer "p2"]

            opaque `shouldBe` Nothing

    describe "Tick" $ do
        it "no-op returns same state" $ do
            let
                state = startGame players <$> mapFromDebug validMap
                players = [mkPlayer "p1", mkPlayer "p2"]

            tick <$> state `shouldBe` state

        context "movement" $ do
            it "up" $ do
                let
                    opaqueInitialState = show . opaqueState <$> initialState
                    opaqueStateWithNextActionQueued = show . opaqueState <$> stateWithNextActionQueued
                    -- opaqueStateAfterTick = show . opaqueState <$> stateAfterTick

                    initialState = startGame players <$> mapFromDebug testMap
                    stateWithNextActionQueued = queueAction player MoveUp <$> initialState
                    -- stateAfterTick = tick <$> stateWithNextActionQueued

                    players = [player]
                    player = mkPlayer "p1"
                    testMap =
                        [ "#####"
                        , "#   #"
                        , "# S #"
                        , "#   #"
                        , "#####"
                        ]

                opaqueInitialState `shouldBe` (Just . intercalate "\n" $
                        [ "#####"
                        , "#   #"
                        , "# 0 #"
                        , "#   #"
                        , "#####"
                        ])

                opaqueStateWithNextActionQueued `shouldBe` (Just . intercalate "\n" $
                        [ "#####"
                        , "#   #"
                        , "# 0 #"
                        , "#   #"
                        , "#####"
                        ])

                pendingWith "implement tick!"
                -- opaqueStateAfterTick `shouldBe` (Just . intercalate "\n" $
                --         [ "#####"
                --         , "# 0 #"
                --         , "#   #"
                --         , "#   #"
                --         , "#####"
                --         ])
