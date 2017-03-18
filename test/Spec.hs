import Test.Hspec
import Data.List
import qualified Data.Sequence as S
import Bombastic

data Input = Input Participant Action

from2dList :: [[a]] -> (S.Seq (S.Seq a))
from2dList = S.fromList . fmap S.fromList

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
                [mkDebugParticipant 1 "p1", mkDebugParticipant 2 "p2"]
                [ "###################"
                , "#1 ..       .... 2#"
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
                getCells (OpaqueState (OpaqueBoard cells2d) _) = cells2d
                opaque = opaqueify . startGame [] <$> mapFromDebug
                    [ "# "
                    , "  "
                    ]

            cells `shouldBe` (Just . from2dList)
                [ [OpaqueIndestructibleBlock, OpaqueEmptyCell]
                , [OpaqueEmptyCell,           OpaqueEmptyCell]
                ]

        it "player number is correct" $ do
            let
                opaque = opaqueify . startGame players <$> mapFromDebug
                    [ "SS"
                    , "S "
                    ]
                players =
                    [ mkDebugParticipant 1 "p1"
                    , mkDebugParticipant 2 "p2"
                    ]
                getPlayers (OpaqueState _ ps) = ps

            (getPlayers <$> opaque) `shouldBe` Just
                [ OpaqueConnectedPlayer (ParticipantName "p1") (Just (Coords 0 0))
                , OpaqueConnectedPlayer (ParticipantName "p2") (Just (Coords 1 0))
                ]


        it "invalid map loads as Nothing" $ do
            let
                opaque = show . opaqueify <$> state
                state = startGame players <$> mapFromDebug invalidMap
                players = [mkDebugParticipant 1 "p1", mkDebugParticipant 2 "p2"]

            opaque `shouldBe` Nothing

    describe "Tick" $ do
        it "no-op returns same state" $ do
            let
                state = startGame players <$> mapFromDebug validMap
                players = [mkDebugParticipant 1 "p1", mkDebugParticipant 2 "p2"]

            tick <$> state `shouldBe` state

        context "movement" $ do
            let
                initialIndestructibleBlockState = startGame players <$>
                    mapFromDebug indestructibleBlockMap

                initialDestructibleBlockState = startGame players <$>
                    mapFromDebug destructibleBlockMap

                players = [player]
                player = mkDebugParticipant 1 "p1"

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
                    , "# 1 #"
                    , "#   #"
                    , "#####"
                    ]
                    [ ( [Input player (Move Up)]
                      , [ "#####"
                        , "# 1 #"
                        , "#   #"
                        , "#   #"
                        , "#####"
                        ]
                      )
                    ]

            it "up against indestructible block" $ do
                doesntMoveWhenTicked (Move Up) initialIndestructibleBlockState

            it "up against destructible block" $ do
                doesntMoveWhenTicked (Move Up) initialDestructibleBlockState

            it "down" $ do
                assertSeries
                    moveMap
                    [player]
                    [ "#####"
                    , "#   #"
                    , "# 1 #"
                    , "#   #"
                    , "#####"
                    ]
                    [ ( [Input player (Move Down)]
                      , [ "#####"
                        , "#   #"
                        , "#   #"
                        , "# 1 #"
                        , "#####"
                        ]
                      )
                    ]

            it "down against indestructible block" $ do
                doesntMoveWhenTicked (Move Down) initialIndestructibleBlockState

            it "down against destructible block" $ do
                doesntMoveWhenTicked (Move Down) initialDestructibleBlockState

            it "left" $ do
                assertSeries
                    moveMap
                    [player]
                    [ "#####"
                    , "#   #"
                    , "# 1 #"
                    , "#   #"
                    , "#####"
                    ]
                    [ ( [Input player (Move Bombastic.Left)]
                      , [ "#####"
                        , "#   #"
                        , "#1  #"
                        , "#   #"
                        , "#####"
                        ]
                      )
                    ]

            it "left against indestructible block" $ do
                doesntMoveWhenTicked (Move Bombastic.Left) initialIndestructibleBlockState

            it "left against destructible block" $ do
                doesntMoveWhenTicked (Move Bombastic.Left) initialDestructibleBlockState

            it "right" $ do
                assertSeries
                    moveMap
                    [player]
                    [ "#####"
                    , "#   #"
                    , "# 1 #"
                    , "#   #"
                    , "#####"
                    ]
                    [ ( [Input player (Move Bombastic.Right)]
                      , [ "#####"
                        , "#   #"
                        , "#  1#"
                        , "#   #"
                        , "#####"
                        ]
                      )
                    ]

            it "right against indestructible block" $ do
                doesntMoveWhenTicked (Move Bombastic.Right) initialIndestructibleBlockState

            it "right against destructible block" $ do
                doesntMoveWhenTicked (Move Bombastic.Right) initialDestructibleBlockState

            it "movement continues" $ do
                assertSeries
                    [ "#####"
                    , "#S  #"
                    , "#####"
                    ]
                    [player]
                    [ "#####"
                    , "#1  #"
                    , "#####"
                    ]
                    [ ( [Input player (Move Bombastic.Right)]
                      , [ "#####"
                        , "# 1 #"
                        , "#####"
                        ]
                      )
                    , ( []
                      , [ "#####"
                        , "#  1#"
                        , "#####"
                        ]
                      )
                    ]

            it "last direction wins" $ do
                assertSeries
                    moveMap
                    [player]
                    [ "#####"
                    , "#   #"
                    , "# 1 #"
                    , "#   #"
                    , "#####"
                    ]
                    [ ( [Input player (Move Bombastic.Left), Input player (Move Bombastic.Right)]
                      , [ "#####"
                        , "#   #"
                        , "#  1#"
                        , "#   #"
                        , "#####"
                        ]
                      )
                    ]
                

        context "bombing" $ do
            let
                player = mkDebugParticipant 1 "p1"

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
                    , "# 1 #"
                    , "#   #"
                    , "#####"
                    ]
                    [ ( [Input player DropBomb]
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
                    , "# 1 #"
                    , "#   #"
                    , "#####"
                    ]
                    [ ( [ Input player DropBomb
                        , Input player (Move Bombastic.Right)
                        ]
                      , [ "#####"
                        , "#   #"
                        , "# Q1#"
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
                    , "# 1 #"
                    , "#   #"
                    , "#####"
                    ]
                    [ ( [ Input player (Move Bombastic.Right)
                        , Input player DropBomb
                        ]
                      , [ "#####"
                        , "#   #"
                        , "# Q1#"
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
                    , "#1  #"
                    , "#####"
                    ]
                    [ ( [ Input player (Move Bombastic.Right)
                        , Input player DropBomb
                        ]
                      , [ "#####"
                        , "#Q1 #"
                        , "#####"
                        ]
                      )
                    , ( []
                      , [ "#####"
                        , "#Q 1#"
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
                    , "# 1 #"
                    , "#####"
                    ]
                    [ ( [ Input player (Move Bombastic.Right)
                        , Input player DropBomb
                        , Input player (Move Bombastic.Left)
                        ]
                      , [ "#####"
                        , "#1Q #"
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
                    , "# 1 #"
                    , "#####"
                    ]
                    [ ( [ Input player (Move Bombastic.Right)
                        , Input player DropBomb
                        , Input player DropBomb
                        ]
                      , [ "#####"
                        , "# Q1#"
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
                    , "#1  #"
                    , "#####"
                    ]
                    [ ( [ Input player (Move Bombastic.Right)
                        , Input player DropBomb
                        ]
                      , [ "#####"
                        , "#Q1 #"
                        , "#####"
                        ]
                      )
                    , ( [Input player (Move Bombastic.Left)]
                      , [ "#####"
                        , "#Q1 #"
                        , "#####"
                        ]
                      )
                    ]

        context "flame" $ do
            let
                player = mkDebugParticipant 1 "p1"

            it "basic bomb detonates after 3 ticks" $ do
                assertSeries
                    [ "#####"
                    , "#   #"
                    , "# S #"
                    , "#   #"
                    , "#####"
                    ]
                    [player]
                    [ "#####"
                    , "#   #"
                    , "# 1 #"
                    , "#   #"
                    , "#####"
                    ]
                    [ ( [Input player DropBomb]
                      , [ "#####"
                        , "#   #"
                        , "# Q #"
                        , "#   #"
                        , "#####"
                        ]
                      )
                    , ( []
                      , [ "#####"
                        , "#   #"
                        , "# Q #"
                        , "#   #"
                        , "#####"
                        ]
                      )
                    , ( []
                      , [ "#####"
                        , "#   #"
                        , "# Q #"
                        , "#   #"
                        , "#####"
                        ]
                      )
                    , ( []
                      , [ "#####"
                        , "# ~ #"
                        , "#~~~#"
                        , "# ~ #"
                        , "#####"
                        ]
                      )
                    , ( []
                      , [ "#####"
                        , "#   #"
                        , "#   #"
                        , "#   #"
                        , "#####"
                        ]
                      )
                    ]

            it "bomb destroys destructible block but not indestructible block" $ do
                assertSeries
                    [ "####"
                    , "#S.#"
                    , "####"
                    ]
                    [player]
                    [ "####"
                    , "#1.#"
                    , "####"
                    ]
                    [ ( [Input player DropBomb]
                      , [ "####"
                        , "#Q.#"
                        , "####"
                        ]
                      )
                    , ( []
                      , [ "####"
                        , "#Q.#"
                        , "####"
                        ]
                      )
                    , ( []
                      , [ "####"
                        , "#Q.#"
                        , "####"
                        ]
                      )
                    , ( []
                      , [ "####"
                        , "#~~#"
                        , "####"
                        ]
                      )
                    , ( []
                      , [ "####"
                        , "#  #"
                        , "####"
                        ]
                      )
                    ]

            it "player can drop another bomb after explosion" $ do
                assertSeries
                    [ "######"
                    , "#S . #"
                    , "# .  #"
                    , "#.   #"
                    , "######"
                    ]
                    [player]
                    [ "######"
                    , "#1 . #"
                    , "# .  #"
                    , "#.   #"
                    , "######"
                    ]
                    [ ( [Input player (Move Down)]
                      , [ "######"
                        , "#  . #"
                        , "#1.  #"
                        , "#.   #"
                        , "######"
                        ]
                      )
                    , ( [Input player DropBomb, Input player (Move Up)]
                      , [ "######"
                        , "#1 . #"
                        , "#Q.  #"
                        , "#.   #"
                        , "######"
                        ]
                      )
                    , ( [Input player (Move Bombastic.Right)]
                      , [ "######"
                        , "# 1. #"
                        , "#Q.  #"
                        , "#.   #"
                        , "######"
                        ]
                      )
                    , ( [Input player DropBomb]
                      , [ "######"
                        , "# 1. #"
                        , "#Q.  #"
                        , "#.   #"
                        , "######"
                        ]
                      )
                    , ( [Input player DropBomb]
                      , [ "######"
                        , "#~Q. #"
                        , "#~~  #"
                        , "#~   #"
                        , "######"
                        ]
                      )
                    ]

        context "powerups" $ do
            let
                player = mkDebugParticipant 1 "p1"

            it "player cannot drop more than one bomb" $ do
                assertSeries
                    [ "#####"
                    , "#S  #"
                    , "#####"
                    ]
                    [player]
                    [ "#####"
                    , "#1  #"
                    , "#####"
                    ]
                    [ ( [ Input player (Move Bombastic.Right)
                        , Input player DropBomb
                        ]
                      , [ "#####"
                        , "#Q1 #"
                        , "#####"
                        ]
                      )
                    , ( [Input player DropBomb]
                      , [ "#####"
                        , "#Q 1#"
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
            --
            --        conclusion: allow having powerups dropped already - it's
            --        not really a problem anyway and removes all complexity. i
            --        can then go about testing the powerup drops statistically
            --        to ensure chance-of-drop is even across b/f/none.

assertSeries :: DebugMap -> [Participant] -> DebugMap -> [([Input], DebugMap)] -> IO ()
assertSeries debugMap names postSpawn expectations = do
    let
        queueAllInputs :: [Input] -> State -> State
        queueAllInputs [] s = s
        queueAllInputs (Input p a:ms) s = queueAllInputs ms (queueAction p a s)

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
