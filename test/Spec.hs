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

-- cellAt :: Coords -> OpaqueState -> Cell
-- cellAt (Coords (x, y)) (OpaqueState (Board cells2d) _ _ _) = (cells2d !! y) !! x

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
                opaque = show . opaqueify <$> state
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

        it "asymmetrical map loads the right way up" $ do
            let
                cells = getCells <$> opaque
                getCells (OpaqueState (Board cells2d) _ _ _) = cells2d
                opaque = opaqueify . startGame [] <$> mapFromDebug
                    [ "# "
                    , "  "
                    ]

            cells `shouldBe` Just
                [ [IndestructibleBlock , EmptyCell]
                , [EmptyCell           , EmptyCell]
                ]

        it "player number is correct" $ do
            let 
                opaque = opaqueify . startGame players <$> mapFromDebug
                    [ "SS"
                    , "S "
                    ]
                players =
                    [ mkPlayer "p1"
                    , mkPlayer "p2"
                    ]
                getPlayers (OpaqueState _ ps _ _) = ps

            (getPlayers <$> opaque) `shouldBe` Just
                [ OpaqueConnectedPlayer (Coords (0, 0))
                , OpaqueConnectedPlayer (Coords (1, 0))
                ]


        it "invalid map loads as Nothing" $ do
            let
                opaque = show . opaqueify <$> state
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
                    opaqueInitialState = show . opaqueify <$> initialState
                    opaqueStateWithNextActionQueued = show . opaqueify <$> stateWithNextActionQueued
                    -- opaqueStateAfterTick = show . opaqueify <$> stateAfterTick

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
