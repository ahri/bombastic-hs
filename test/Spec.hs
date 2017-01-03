import Test.Hspec
import Data.List
import Bombastic

data Input = Input PlayerId Action

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
                [mkDebugPlayer 1 "p1", mkDebugPlayer 2 "p2"]
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
                    [ mkDebugPlayer 1 "p1"
                    , mkDebugPlayer 2 "p2"
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
                players = [mkDebugPlayer 1 "p1", mkDebugPlayer 2 "p2"]

            opaque `shouldBe` Nothing

    describe "Tick" $ do
        it "no-op returns same state" $ do
            let
                state = startGame players <$> mapFromDebug validMap
                players = [mkDebugPlayer 1 "p1", mkDebugPlayer 2 "p2"]

            tick <$> state `shouldBe` state

        context "movement" $ do
            let
                initialIndestructibleBlockState = startGame players <$>
                    mapFromDebug indestructibleBlockMap

                initialDestructibleBlockState = startGame players <$>
                    mapFromDebug destructibleBlockMap

                players = [player]
                player = mkDebugPlayer 1 "p1"
                pid = getPlayerId player

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
                    (opaqueify . tick . queueAction pid action <$> state) `shouldBe` (opaqueify <$> state)

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
                    [ ( [Input pid MoveUp]
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
                    [ ( [Input pid MoveDown]
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
                    [ ( [Input pid MoveLeft]
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
                    [ ( [Input pid MoveLeft, Input pid MoveRight]
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

            it "movement continues" $ do
                assertSeries
                    [ "#####"
                    , "#S  #"
                    , "#####"
                    ]
                    [player]
                    [ "#####"
                    , "#0  #"
                    , "#####"
                    ]
                    [ ( [Input pid MoveRight]
                      , [ "#####"
                        , "# 0 #"
                        , "#####"
                        ]
                      )
                    , ( []
                      , [ "#####"
                        , "#  0#"
                        , "#####"
                        ]
                      )
                    ]
                

        context "bombing" $ do
            let
                player = mkDebugPlayer 1 "p1"
                pid = getPlayerId player

                moveMap =
                    [ "#####"
                    , "#   #"
                    , "# S #"
                    , "#   #"
                    , "#####"
                    ]

            it "a bomb is dropped" $ do
                assertSeries
                    moveMap
                    [player]
                    [ "#####"
                    , "#   #"
                    , "# 0 #"
                    , "#   #"
                    , "#####"
                    ]
                    [ ( [Input pid DropBomb]
                      , [ "#####"
                        , "#   #"
                        , "# Q #"
                        , "#   #"
                        , "#####"
                        ]
                      )
                    ]

            it "a bomb is dropped and movement occurs" $ do
                assertSeries
                    moveMap
                    [player]
                    [ "#####"
                    , "#   #"
                    , "# 0 #"
                    , "#   #"
                    , "#####"
                    ]
                    [ ( [ Input pid DropBomb
                        , Input pid MoveRight
                        ]
                      , [ "#####"
                        , "#   #"
                        , "# Q0#"
                        , "#   #"
                        , "#####"
                        ]
                      )
                    ]

            it "movement occurs and a bomb is dropped" $ do
                assertSeries
                    moveMap
                    [player]
                    [ "#####"
                    , "#   #"
                    , "# 0 #"
                    , "#   #"
                    , "#####"
                    ]
                    [ ( [ Input pid MoveRight
                        , Input pid DropBomb
                        ]
                      , [ "#####"
                        , "#   #"
                        , "# Q0#"
                        , "#   #"
                        , "#####"
                        ]
                      )
                    ]

            it "movement continues without bombs dropping" $ do
                assertSeries
                    [ "#####"
                    , "#S  #"
                    , "#####"
                    ]
                    [player]
                    [ "#####"
                    , "#0  #"
                    , "#####"
                    ]
                    [ ( [ Input pid MoveRight
                        , Input pid DropBomb
                        ]
                      , [ "#####"
                        , "#Q0 #"
                        , "#####"
                        ]
                      )
                    , ( []
                      , [ "#####"
                        , "#Q 0#"
                        , "#####"
                        ]
                      )
                    ]

            it "change of mind in direction still drops bomb" $ do
                assertSeries
                    [ "#####"
                    , "# S #"
                    , "#####"
                    ]
                    [player]
                    [ "#####"
                    , "# 0 #"
                    , "#####"
                    ]
                    [ ( [ Input pid MoveRight
                        , Input pid DropBomb
                        , Input pid MoveLeft
                        ]
                      , [ "#####"
                        , "#0Q #"
                        , "#####"
                        ]
                      )
                    ]

            it "repeated bomb action doesn't change anything" $ do
                assertSeries
                    [ "#####"
                    , "# S #"
                    , "#####"
                    ]
                    [player]
                    [ "#####"
                    , "# 0 #"
                    , "#####"
                    ]
                    [ ( [ Input pid MoveRight
                        , Input pid DropBomb
                        , Input pid DropBomb
                        ]
                      , [ "#####"
                        , "# Q0#"
                        , "#####"
                        ]
                      )
                    ]

            it "player cannot move onto bomb" $ do
                assertSeries
                    [ "#####"
                    , "#S  #"
                    , "#####"
                    ]
                    [player]
                    [ "#####"
                    , "#0  #"
                    , "#####"
                    ]
                    [ ( [ Input pid MoveRight
                        , Input pid DropBomb
                        ]
                      , [ "#####"
                        , "#Q0 #"
                        , "#####"
                        ]
                      )
                    , ( [Input pid MoveLeft]
                      , [ "#####"
                        , "#Q0 #"
                        , "#####"
                        ]
                      )
                    ]

        context "powerups" $ do
            let
                player = mkDebugPlayer 1 "p1"
                pid = getPlayerId player

            it "player cannot drop more than one bomb" $ do
                assertSeries
                    [ "#####"
                    , "#S  #"
                    , "#####"
                    ]
                    [player]
                    [ "#####"
                    , "#0  #"
                    , "#####"
                    ]
                    [ ( [ Input pid MoveRight
                        , Input pid DropBomb
                        ]
                      , [ "#####"
                        , "#Q0 #"
                        , "#####"
                        ]
                      )
                    , ( [Input pid DropBomb]
                      , [ "#####"
                        , "#Q 0#"
                        , "#####"
                        ]
                      )
                    ]

            --- TODO: i want to test that extra flame and bomb powerups affect
            --        how long flame is, and how many bombs can be dropped, but
            --        due to the randomness it feels like the only ways to
            --        achieve this in a test are:
            --          1. set a state via some hopefully-debug-only api
            --          2. pick random seeds until one suits me, and use that to
            --             ensure a specific powerup
            --        - seems like (1) is more sensible... but also a bit risky.
            --          maybe see if modules can help us expose only certain
            --          functionality for test purposes? that'd be nice

assertSeries :: DebugMap -> [Participant] -> DebugMap -> [([Input], DebugMap)] -> IO ()
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
