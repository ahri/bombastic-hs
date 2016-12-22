import Test.Hspec
import Data.List
import Bombastic

data Input = Input PlayerName Action

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
            assertSeries
                validMap
                [PlayerName "p1", PlayerName "p2"]
                [ "###################"
                , "#0 ..       .... 1#"
                , "# # # # # # #.#.# #"
                , "#..       # ......#"
                , "# # # # ##### # # #"
                , "#         #       #"
                , "###################"
                ]
                []

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
                    [ PlayerName "p1"
                    , PlayerName "p2"
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
                players = [PlayerName "p1", PlayerName "p2"]

            opaque `shouldBe` Nothing

    describe "Tick" $ do
        it "no-op returns same state" $ do
            let
                state = startGame players <$> mapFromDebug validMap
                players = [PlayerName "p1", PlayerName "p2"]

            tick <$> state `shouldBe` state

        context "movement" $ do
            let
                initialIndestructibleBlockState = startGame players <$>
                    mapFromDebug indestructibleBlockMap

                initialDestructibleBlockState = startGame players <$>
                    mapFromDebug destructibleBlockMap

                players = [player]
                player = PlayerName "p1"
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

            it "up" $ do
                assertSeries
                    moveMap
                    [player]
                    [ "#####"
                    , "#   #"
                    , "# 0 #"
                    , "#   #"
                    , "#####"
                    ]
                    [ ( [Input player MoveUp]
                      , [ "#####"
                        , "# 0 #"
                        , "#   #"
                        , "#   #"
                        , "#####"
                        ]
                      )
                    ]

            it "up against indestructible block" $ do
                doesntMoveWhenTicked MoveUp initialIndestructibleBlockState

            it "up against destructible block" $ do
                doesntMoveWhenTicked MoveUp initialDestructibleBlockState

            it "down" $ do
                assertSeries
                    moveMap
                    [player]
                    [ "#####"
                    , "#   #"
                    , "# 0 #"
                    , "#   #"
                    , "#####"
                    ]
                    [ ( [Input player MoveDown]
                      , [ "#####"
                        , "#   #"
                        , "#   #"
                        , "# 0 #"
                        , "#####"
                        ]
                      )
                    ]

            it "down against indestructible block" $ do
                doesntMoveWhenTicked MoveDown initialIndestructibleBlockState

            it "down against destructible block" $ do
                doesntMoveWhenTicked MoveDown initialDestructibleBlockState

            it "left" $ do
                assertSeries
                    moveMap
                    [player]
                    [ "#####"
                    , "#   #"
                    , "# 0 #"
                    , "#   #"
                    , "#####"
                    ]
                    [ ( [Input player MoveLeft]
                      , [ "#####"
                        , "#   #"
                        , "#0  #"
                        , "#   #"
                        , "#####"
                        ]
                      )
                    ]

            it "left against indestructible block" $ do
                doesntMoveWhenTicked MoveLeft initialIndestructibleBlockState

            it "left against destructible block" $ do
                doesntMoveWhenTicked MoveLeft initialDestructibleBlockState

            it "right" $ do
                assertSeries
                    moveMap
                    [player]
                    [ "#####"
                    , "#   #"
                    , "# 0 #"
                    , "#   #"
                    , "#####"
                    ]
                    [ ( [Input player MoveRight]
                      , [ "#####"
                        , "#   #"
                        , "#  0#"
                        , "#   #"
                        , "#####"
                        ]
                      )
                    ]

            it "right against indestructible block" $ do
                doesntMoveWhenTicked MoveRight initialIndestructibleBlockState

            it "right against destructible block" $ do
                doesntMoveWhenTicked MoveRight initialDestructibleBlockState


assertSeries :: DebugMap -> [PlayerName] -> DebugMap -> [([Input], DebugMap)] -> IO ()
assertSeries debugMap names postSpawn expectations = do
    let
        queueAllInputs :: [Input] -> State -> State
        queueAllInputs [] s = s
        queueAllInputs (Input pn a:ms) s = queueAllInputs ms (queueAction pn a s)

        assertOnInitial Nothing = return ()
        assertOnInitial (Just s) = (show . opaqueify $ s) `shouldBe` intercalate "\n" postSpawn

        go :: [([Input], DebugMap)] -> Maybe State -> IO ()
        go _ Nothing = return ()
        go [] _ = return ()
        go ((ms, e):xs) (Just s) = do
            let
                opaqueNewState = opaqueify newState
                newState = tick . queueAllInputs ms $ s

            show opaqueNewState `shouldBe` intercalate "\n" e
            go xs (Just newState)

        initialState = startGame names <$> mapFromDebug debugMap

    assertOnInitial initialState
    go expectations initialState
