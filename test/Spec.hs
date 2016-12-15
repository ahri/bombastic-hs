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
            let
                initialMoveState = startGame players <$>
                    mapFromDebug moveMap

                initialIndestructibleBlockState = startGame players <$>
                    mapFromDebug indestructibleBlockMap

                initialDestructibleBlockState = startGame players <$>
                    mapFromDebug destructibleBlockMap

                players = [player]
                player = mkPlayer "p1"
                moveMap =
                    [ "#####"
                    , "#   #"
                    , "# S #"
                    , "#   #"
                    , "#####"
                    ]

                indestructibleBlockMap =
                    [ "###"
                    , "#S#"
                    , "###"
                    ]

                destructibleBlockMap =
                    [ "..."
                    , ".S."
                    , "..."
                    ]

                doesntMoveWhenTicked action state =
                    (opaqueify . tick . queueAction player action <$> state) `shouldBe` (opaqueify <$> state)

                movesWhenTicked action state expectation = do
                    let
                        opaqueStateWithNextActionQueued = show . opaqueify <$> stateWithNextActionQueued
                        opaqueStateAfterTick = show . opaqueify <$> stateAfterTick

                        stateWithNextActionQueued = queueAction player action <$> state
                        stateAfterTick = tick <$> stateWithNextActionQueued

                    opaqueStateWithNextActionQueued `shouldBe` (Just . intercalate "\n" $
                            [ "#####"
                            , "#   #"
                            , "# 0 #"
                            , "#   #"
                            , "#####"
                            ])

                    opaqueStateAfterTick `shouldBe` (Just . intercalate "\n" $ expectation)

            it "up" $ do
                movesWhenTicked MoveUp initialMoveState 
                        [ "#####"
                        , "# 0 #"
                        , "#   #"
                        , "#   #"
                        , "#####"
                        ]

            it "up against indestructible block" $ do
                doesntMoveWhenTicked MoveUp initialIndestructibleBlockState

            it "up against destructible block" $ do
                doesntMoveWhenTicked MoveUp initialDestructibleBlockState

            it "down" $ do
                movesWhenTicked MoveDown initialMoveState 
                        [ "#####"
                        , "#   #"
                        , "#   #"
                        , "# 0 #"
                        , "#####"
                        ]

            it "down against indestructible block" $ do
                doesntMoveWhenTicked MoveDown initialIndestructibleBlockState

            it "down against destructible block" $ do
                doesntMoveWhenTicked MoveDown initialDestructibleBlockState

            it "left" $ do
                movesWhenTicked MoveLeft initialMoveState 
                        [ "#####"
                        , "#   #"
                        , "#0  #"
                        , "#   #"
                        , "#####"
                        ]

            it "left against indestructible block" $ do
                doesntMoveWhenTicked MoveLeft initialIndestructibleBlockState

            it "left against destructible block" $ do
                doesntMoveWhenTicked MoveLeft initialDestructibleBlockState

            it "right" $ do
                movesWhenTicked MoveRight initialMoveState 
                        [ "#####"
                        , "#   #"
                        , "#  0#"
                        , "#   #"
                        , "#####"
                        ]

            it "right against indestructible block" $ do
                doesntMoveWhenTicked MoveRight initialIndestructibleBlockState

            it "right against destructible block" $ do
                doesntMoveWhenTicked MoveRight initialDestructibleBlockState
