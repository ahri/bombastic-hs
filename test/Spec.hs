{-
 - TODO:
 -  * quit works
 -  * end-game works; maybe GameState = Ongoing ... | Finished (Maybe Participant)
 -        strategy: rename GameState to Ongoing. create union. fix errors
 -  * flame has direction (for rendering purposes)
 -}

import Test.Hspec
import Data.List
import System.Random
import qualified Data.Sequence as S
import Bombastic

data Input = Input Participant Action

from2dList :: [[a]] -> (S.Seq (S.Seq a))
from2dList = S.fromList . fmap S.fromList

validMap :: [String]
validMap =
    [ "###################"
    , "#S ++       ++++ S#"
    , "# # # # # # #+#+# #"
    , "#++       # ++++++#"
    , "# # # # ##### # # #"
    , "#S        #      S#"
    , "###################"
    ]

-- cellAt :: Coords -> OpaqueGameState -> Cell
-- cellAt (Coords (x, y)) (OpaqueGameState (Board cells2d) _ _ _) = (cells2d !! y) !! x

explosionResultNoPowerup :: StdGen -> (StdGen, Cell)
explosionResultNoPowerup g = (g, EmptyCell)

main :: IO ()
main = do

  g <- getStdGen
  hspec $ do -- TODO: re-indent? i needed to be in the IO() monad to get g :\

    describe "Map load" $ do
        let
            invalidMap =
                [ "######"
                , "#S+ !#"
                , "#+S++#"
                , "#++ S#"
                , "######"
                ]

        it "valid map loads correctly" $ do
            assertSeries
                validMap
                [mkDebugParticipant 1 "p1", mkDebugParticipant 2 "p2"]
                g
                explosionResultNoPowerup
                [ "###################"
                , "#1 ++       ++++ 2#"
                , "# # # # # # #+#+# #"
                , "#++       # ++++++#"
                , "# # # # ##### # # #"
                , "#         #       #"
                , "###################"
                ]
                []

        it "asymmetrical map loads the right way up" $ do
            let
                cells = getCells <$> opaque
                getCells (OpaqueGameState (OpaqueBoard cells2d) _) = cells2d
                opaque = opaqueify . startGame [] g explosionResultNoPowerup <$> mapFromDebug
                    [ "# "
                    , "  "
                    ]

            cells `shouldBe` (Just . from2dList)
                [ [OpaqueIndestructibleBlock, OpaqueEmptyCell]
                , [OpaqueEmptyCell,           OpaqueEmptyCell]
                ]

        it "player number is correct" $ do
            let
                opaque = opaqueify . startGame players g explosionResultNoPowerup <$> mapFromDebug
                    [ "SS"
                    , "S "
                    ]
                players =
                    [ mkDebugParticipant 1 "p1"
                    , mkDebugParticipant 2 "p2"
                    ]
                getPlayers (OpaqueGameState _ ps) = ps

            (getPlayers <$> opaque) `shouldBe` Just
                [ OpaqueConnectedPlayer (ParticipantName "p1") (Coords 0 0)
                , OpaqueConnectedPlayer (ParticipantName "p2") (Coords 1 0)
                ]


        it "invalid map loads as Nothing" $ do
            let
                opaque = show . opaqueify <$> state
                state = startGame players g explosionResultNoPowerup <$> mapFromDebug invalidMap
                players = [mkDebugParticipant 1 "p1", mkDebugParticipant 2 "p2"]

            opaque `shouldBe` Nothing

    describe "Tick" $ do
        it "no-op returns same state" $ do
            let
                state = startGame players g explosionResultNoPowerup <$> mapFromDebug validMap
                players = [mkDebugParticipant 1 "p1", mkDebugParticipant 2 "p2"]

            tick <$> state `shouldBe` state

        context "movement" $ do
            let
                initialIndestructibleBlockGameState = startGame players g explosionResultNoPowerup <$>
                    mapFromDebug indestructibleBlockMap

                initialDestructibleBlockGameState = startGame players g explosionResultNoPowerup <$>
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
                    [ "+++"
                    , "+S+"
                    , "+++"
                    ]

                doesntMoveWhenTicked action state =
                    (opaqueify . tick . queueAction player action <$> state) `shouldBe` (opaqueify <$> state)

            it "up" $ do
                assertSeries
                    moveMap
                    [player]
                    g
                    explosionResultNoPowerup
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
                doesntMoveWhenTicked (Move Up) initialIndestructibleBlockGameState

            it "up against destructible block" $ do
                doesntMoveWhenTicked (Move Up) initialDestructibleBlockGameState

            it "down" $ do
                assertSeries
                    moveMap
                    [player]
                    g
                    explosionResultNoPowerup
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
                doesntMoveWhenTicked (Move Down) initialIndestructibleBlockGameState

            it "down against destructible block" $ do
                doesntMoveWhenTicked (Move Down) initialDestructibleBlockGameState

            it "left" $ do
                assertSeries
                    moveMap
                    [player]
                    g
                    explosionResultNoPowerup
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
                doesntMoveWhenTicked (Move Bombastic.Left) initialIndestructibleBlockGameState

            it "left against destructible block" $ do
                doesntMoveWhenTicked (Move Bombastic.Left) initialDestructibleBlockGameState

            it "right" $ do
                assertSeries
                    moveMap
                    [player]
                    g
                    explosionResultNoPowerup
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
                doesntMoveWhenTicked (Move Bombastic.Right) initialIndestructibleBlockGameState

            it "right against destructible block" $ do
                doesntMoveWhenTicked (Move Bombastic.Right) initialDestructibleBlockGameState

            it "movement continues" $ do
                assertSeries
                    [ "#####"
                    , "#S  #"
                    , "#####"
                    ]
                    [player]
                    g
                    explosionResultNoPowerup
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
                    g
                    explosionResultNoPowerup
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
                    g
                    explosionResultNoPowerup
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
                    g
                    explosionResultNoPowerup
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
                    g
                    explosionResultNoPowerup
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
                    g
                    explosionResultNoPowerup
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
                    g
                    explosionResultNoPowerup
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
                    g
                    explosionResultNoPowerup
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
                    g
                    explosionResultNoPowerup
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
                    [ "#######"
                    , "#     #"
                    , "#     #"
                    , "#  S  #"
                    , "#     #"
                    , "#     #"
                    , "#######"
                    ]
                    [player]
                    g
                    explosionResultNoPowerup
                    [ "#######"
                    , "#     #"
                    , "#     #"
                    , "#  1  #"
                    , "#     #"
                    , "#     #"
                    , "#######"
                    ]
                    [ ( [Input player DropBomb]
                      , [ "#######"
                        , "#     #"
                        , "#     #"
                        , "#  Q  #"
                        , "#     #"
                        , "#     #"
                        , "#######"
                        ]
                      )
                    , ( []
                      , [ "#######"
                        , "#     #"
                        , "#     #"
                        , "#  Q  #"
                        , "#     #"
                        , "#     #"
                        , "#######"
                        ]
                      )
                    , ( []
                      , [ "#######"
                        , "#     #"
                        , "#     #"
                        , "#  Q  #"
                        , "#     #"
                        , "#     #"
                        , "#######"
                        ]
                      )
                    , ( []
                      , [ "#######"
                        , "#     #"
                        , "#  ~  #"
                        , "# ~~~ #"
                        , "#  ~  #"
                        , "#     #"
                        , "#######"
                        ]
                      )
                    , ( []
                      , [ "#######"
                        , "#     #"
                        , "#     #"
                        , "#     #"
                        , "#     #"
                        , "#     #"
                        , "#######"
                        ]
                      )
                    ]

            it "bomb explosion kills player" $ do
                assertSeries
                    [ "####"
                    , "#S #"
                    , "####"
                    ]
                    [player]
                    g
                    explosionResultNoPowerup
                    [ "####"
                    , "#1 #"
                    , "####"
                    ]
                    [ ( [Input player DropBomb]
                      , [ "####"
                        , "#Q #"
                        , "####"
                        ]
                      )
                    , ( []
                      , [ "####"
                        , "#Q #"
                        , "####"
                        ]
                      )
                    , ( []
                      , [ "####"
                        , "#Q #"
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

            it "bomb flame kills player" $ do
                assertSeries
                    [ "####"
                    , "#S #"
                    , "####"
                    ]
                    [player]
                    g
                    explosionResultNoPowerup
                    [ "####"
                    , "#1 #"
                    , "####"
                    ]
                    [ ( [Input player DropBomb, Input player (Move Bombastic.Right)]
                      , [ "####"
                        , "#Q1#"
                        , "####"
                        ]
                      )
                    , ( []
                      , [ "####"
                        , "#Q1#"
                        , "####"
                        ]
                      )
                    , ( []
                      , [ "####"
                        , "#Q1#"
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

            it "bomb destroys destructible block but not indestructible block" $ do
                assertSeries
                    [ "####"
                    , "#S+#"
                    , "####"
                    ]
                    [player]
                    g
                    explosionResultNoPowerup
                    [ "####"
                    , "#1+#"
                    , "####"
                    ]
                    [ ( [Input player DropBomb]
                      , [ "####"
                        , "#Q+#"
                        , "####"
                        ]
                      )
                    , ( []
                      , [ "####"
                        , "#Q+#"
                        , "####"
                        ]
                      )
                    , ( []
                      , [ "####"
                        , "#Q+#"
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

            it "flame is not inhibited by a powerup, but destructible blocks do inhibit it" $ do
                assertSeries
                    [ "######"
                    , "#S++ #"
                    , "#    #"
                    , "######"
                    ]
                    [player]
                    g
                    (\g' -> (g', Powerup FlamePowerup))
                    [ "######"
                    , "#1++ #"
                    , "#    #"
                    , "######"
                    ]
                    [ ( [Input player DropBomb, Input player (Move Down)]
                      , [ "######"
                        , "#Q++ #"
                        , "#1   #"
                        , "######"
                        ]
                      )
                    , ( [Input player (Move Bombastic.Right)]
                      , [ "######"
                        , "#Q++ #"
                        , "# 1  #"
                        , "######"
                        ]
                      )
                    , ( [Input player (Move Down)]
                      , [ "######"
                        , "#Q++ #"
                        , "# 1  #"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "#~~+ #"
                        , "#~1  #"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "# f+ #"
                        , "# 1  #"
                        , "######"
                        ]
                      )
                    , ( [Input player (Move Up)]
                      , [ "######"
                        , "# 1+ #"
                        , "#    #"
                        , "######"
                        ]
                      )
                    , ( [Input player DropBomb, Input player (Move Down)]
                      , [ "######"
                        , "# Q+ #"
                        , "# 1  #"
                        , "######"
                        ]
                      )
                    , ( [Input player (Move Bombastic.Right)]
                      , [ "######"
                        , "# Q+ #"
                        , "#  1 #"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "# Q+ #"
                        , "#   1#"
                        , "######"
                        ]
                      )
                    , ( [Input player (Move Up)]
                      , [ "######"
                        , "#~~~1#"
                        , "# ~  #"
                        , "######"
                        ]
                      )
                    , ( [Input player DropBomb]
                      , [ "######"
                        , "#  fQ#"
                        , "#    #"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "#  fQ#"
                        , "#    #"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "#  fQ#"
                        , "#    #"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "# ~~~#"
                        , "#   ~#"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "#    #"
                        , "#    #"
                        , "######"
                        ]
                      )
                    ]

            it "flames cross over" $ do
                assertSeries
                    [ "#######"
                    , "#Sbbf #"
                    , "#     #"
                    , "#     #"
                    , "#######"
                    ]
                    [player]
                    g
                    explosionResultNoPowerup
                    [ "#######"
                    , "#1bbf #"
                    , "#     #"
                    , "#     #"
                    , "#######"
                    ]
                    [ ( [Input player (Move Bombastic.Right)]
                      , [ "#######"
                        , "# 1bf #"
                        , "#     #"
                        , "#     #"
                        , "#######"
                        ]
                      )
                    , ( []
                      , [ "#######"
                        , "#  1f #"
                        , "#     #"
                        , "#     #"
                        , "#######"
                        ]
                      )
                    , ( []
                      , [ "#######"
                        , "#   1 #"
                        , "#     #"
                        , "#     #"
                        , "#######"
                        ]
                      )
                    , ( [Input player DropBomb]
                      , [ "#######"
                        , "#   Q1#"
                        , "#     #"
                        , "#     #"
                        , "#######"
                        ]
                      )
                    , ( [Input player DropBomb, Input player (Move Down)]
                      , [ "#######"
                        , "#   QQ#"
                        , "#    1#"
                        , "#     #"
                        , "#######"
                        ]
                      )
                    , ( [Input player DropBomb]
                      , [ "#######"
                        , "#   QQ#"
                        , "#    Q#"
                        , "#    1#"
                        , "#######"
                        ]
                      )
                    , ( [Input player DropBomb]
                      , [ "#######"
                        , "# ~~~~#"
                        , "#  ~~~#"
                        , "#   ~~#"
                        , "#######"
                        ]
                      )
                    ]

            it "flames cross over pending powerup drops" $ do
                assertSeries
                    [ "#######"
                    , "#Sbbf #"
                    , "#   + #"
                    , "#     #"
                    , "#######"
                    ]
                    [player]
                    g
                    (\g' -> (g', Powerup FlamePowerup))
                    [ "#######"
                    , "#1bbf #"
                    , "#   + #"
                    , "#     #"
                    , "#######"
                    ]
                    [ ( [Input player (Move Bombastic.Right)]
                      , [ "#######"
                        , "# 1bf #"
                        , "#   + #"
                        , "#     #"
                        , "#######"
                        ]
                      )
                    , ( []
                      , [ "#######"
                        , "#  1f #"
                        , "#   + #"
                        , "#     #"
                        , "#######"
                        ]
                      )
                    , ( []
                      , [ "#######"
                        , "#   1 #"
                        , "#   + #"
                        , "#     #"
                        , "#######"
                        ]
                      )
                    , ( [Input player DropBomb]
                      , [ "#######"
                        , "#   Q1#"
                        , "#   + #"
                        , "#     #"
                        , "#######"
                        ]
                      )
                    , ( [Input player DropBomb, Input player (Move Down)]
                      , [ "#######"
                        , "#   QQ#"
                        , "#   +1#"
                        , "#     #"
                        , "#######"
                        ]
                      )
                    , ( [Input player DropBomb]
                      , [ "#######"
                        , "#   QQ#"
                        , "#   +Q#"
                        , "#    1#"
                        , "#######"
                        ]
                      )
                    , ( []
                      , [ "#######"
                        , "# ~~~~#"
                        , "#  ~~~#"
                        , "#    ~#"
                        , "#######"
                        ]
                      )
                    , ( []
                      , [ "#######"
                        , "#     #"
                        , "#   f #"
                        , "#     #"
                        , "#######"
                        ]
                      )
                    ]

            it "hammering drop bomb makes no difference if player dies" $ do
                assertSeries
                    [ "####"
                    , "#S #"
                    , "####"
                    ]
                    [player]
                    g
                    explosionResultNoPowerup
                    [ "####"
                    , "#1 #"
                    , "####"
                    ]
                    [ ( [Input player DropBomb, Input player (Move Bombastic.Right)]
                      , [ "####"
                        , "#Q1#"
                        , "####"
                        ]
                      )
                    , ( [Input player DropBomb]
                      , [ "####"
                        , "#Q1#"
                        , "####"
                        ]
                      )
                    , ( [Input player DropBomb]
                      , [ "####"
                        , "#Q1#"
                        , "####"
                        ]
                      )
                    , ( [Input player DropBomb]
                      , [ "####"
                        , "#~~#"
                        , "####"
                        ]
                      )
                    , ( [Input player DropBomb]
                      , [ "####"
                        , "#  #"
                        , "####"
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
                    g
                    explosionResultNoPowerup
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

            it "player can drop another bomb after explosion" $ do
                assertSeries
                    [ "######"
                    , "#S + #"
                    , "# +  #"
                    , "#+   #"
                    , "######"
                    ]
                    [player]
                    g
                    explosionResultNoPowerup
                    [ "######"
                    , "#1 + #"
                    , "# +  #"
                    , "#+   #"
                    , "######"
                    ]
                    [ ( [Input player (Move Down)]
                      , [ "######"
                        , "#  + #"
                        , "#1+  #"
                        , "#+   #"
                        , "######"
                        ]
                      )
                    , ( [Input player DropBomb, Input player (Move Up)]
                      , [ "######"
                        , "#1 + #"
                        , "#Q+  #"
                        , "#+   #"
                        , "######"
                        ]
                      )
                    , ( [Input player (Move Bombastic.Right)]
                      , [ "######"
                        , "# 1+ #"
                        , "#Q+  #"
                        , "#+   #"
                        , "######"
                        ]
                      )
                    , ( [Input player DropBomb]
                      , [ "######"
                        , "# 1+ #"
                        , "#Q+  #"
                        , "#+   #"
                        , "######"
                        ]
                      )
                    , ( [Input player DropBomb]
                      , [ "######"
                        , "#~Q+ #"
                        , "#~~  #"
                        , "#~   #"
                        , "######"
                        ]
                      )
                    ]

            it "can drop flame powerup from tests" $ do
                assertSeries
                    [ "####"
                    , "#S+#"
                    , "####"
                    ]
                    [player]
                    g
                    (\g' -> (g', Powerup FlamePowerup))
                    [ "####"
                    , "#1+#"
                    , "####"
                    ]
                    [ ( [Input player DropBomb]
                      , [ "####"
                        , "#Q+#"
                        , "####"
                        ]
                      )
                    , ( []
                      , [ "####"
                        , "#Q+#"
                        , "####"
                        ]
                      )
                    , ( []
                      , [ "####"
                        , "#Q+#"
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
                        , "# f#"
                        , "####"
                        ]
                      )
                    ]

            it "flame powerup increases flame length" $ do
                assertSeries
                    [ "#####"
                    , "# f #"
                    , "# S #"
                    , "#####"
                    ]
                    [player]
                    g
                    explosionResultNoPowerup
                    [ "#####"
                    , "# f #"
                    , "# 1 #"
                    , "#####"
                    ]
                    [ ( [Input player (Move Up)]
                      , [ "#####"
                        , "# 1 #"
                        , "#   #"
                        , "#####"
                        ]
                      )
                    , ( [Input player (Move Bombastic.Right)]
                      , [ "#####"
                        , "#  1#"
                        , "#   #"
                        , "#####"
                        ]
                      )
                    , ( [Input player DropBomb, Input player (Move Down)]
                      , [ "#####"
                        , "#  Q#"
                        , "#  1#"
                        , "#####"
                        ]
                      )
                    , ( []
                      , [ "#####"
                        , "#  Q#"
                        , "#  1#"
                        , "#####"
                        ]
                      )
                    , ( []
                      , [ "#####"
                        , "#  Q#"
                        , "#  1#"
                        , "#####"
                        ]
                      )
                    , ( []
                      , [ "#####"
                        , "#~~~#"
                        , "#  ~#"
                        , "#####"
                        ]
                      )
                    ]

            it "flame powerup increases flame length, when doing a 'BombMove' onto the powerup" $ do
                assertSeries
                    [ "######"
                    , "#  f #"
                    , "#  S #"
                    , "######"
                    ]
                    [player]
                    g
                    explosionResultNoPowerup
                    [ "######"
                    , "#  f #"
                    , "#  1 #"
                    , "######"
                    ]
                    [ ( [Input player DropBomb, Input player (Move Up)]
                      , [ "######"
                        , "#  1 #"
                        , "#  Q #"
                        , "######"
                        ]
                      )
                    , ( [Input player (Move Bombastic.Right)]
                      , [ "######"
                        , "#   1#"
                        , "#  Q #"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "#   1#"
                        , "#  Q #"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "#  ~1#"
                        , "# ~~~#"
                        , "######"
                        ]
                      )
                    , ( [Input player DropBomb, Input player (Move Down)]
                      , [ "######"
                        , "#   Q#"
                        , "#   1#"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "#   Q#"
                        , "#   1#"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "#   Q#"
                        , "#   1#"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "# ~~~#"
                        , "#   ~#"
                        , "######"
                        ]
                      )
                    ]

            it "flame powerup is destructible" $ do
                assertSeries
                    [ "#####"
                    , "# f #"
                    , "# S #"
                    , "#####"
                    ]
                    [player]
                    g
                    explosionResultNoPowerup
                    [ "#####"
                    , "# f #"
                    , "# 1 #"
                    , "#####"
                    ]
                    [ ( [Input player DropBomb]
                      , [ "#####"
                        , "# f #"
                        , "# Q #"
                        , "#####"
                        ]
                      )
                    , ( []
                      , [ "#####"
                        , "# f #"
                        , "# Q #"
                        , "#####"
                        ]
                      )
                    , ( []
                      , [ "#####"
                        , "# f #"
                        , "# Q #"
                        , "#####"
                        ]
                      )
                    , ( []
                      , [ "#####"
                        , "# ~ #"
                        , "#~~~#"
                        , "#####"
                        ]
                      )
                    , ( []
                      , [ "#####"
                        , "#   #"
                        , "#   #"
                        , "#####"
                        ]
                      )
                    ]

            it "can drop bomb powerup from tests" $ do
                assertSeries
                    [ "####"
                    , "#S+#"
                    , "####"
                    ]
                    [player]
                    g
                    (\g' -> (g', Powerup BombPowerup))
                    [ "####"
                    , "#1+#"
                    , "####"
                    ]
                    [ ( [Input player DropBomb]
                      , [ "####"
                        , "#Q+#"
                        , "####"
                        ]
                      )
                    , ( []
                      , [ "####"
                        , "#Q+#"
                        , "####"
                        ]
                      )
                    , ( []
                      , [ "####"
                        , "#Q+#"
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
                        , "# b#"
                        , "####"
                        ]
                      )
                    ]

            it "bomb powerup increases number of bombs you can drop" $ do
                assertSeries
                    [ "#####"
                    , "# b #"
                    , "# S #"
                    , "#####"
                    ]
                    [player]
                    g
                    explosionResultNoPowerup
                    [ "#####"
                    , "# b #"
                    , "# 1 #"
                    , "#####"
                    ]
                    [ ( [Input player (Move Up)]
                      , [ "#####"
                        , "# 1 #"
                        , "#   #"
                        , "#####"
                        ]
                      )
                    , ( [Input player (Move Bombastic.Right)]
                      , [ "#####"
                        , "#  1#"
                        , "#   #"
                        , "#####"
                        ]
                      )
                    , ( [Input player DropBomb, Input player (Move Down)]
                      , [ "#####"
                        , "#  Q#"
                        , "#  1#"
                        , "#####"
                        ]
                      )
                    , ( [Input player DropBomb]
                      , [ "#####"
                        , "#  Q#"
                        , "#  Q#"
                        , "#####"
                        ]
                      )
                    ]

            it "bomb powerup increases number of bombs you can drop, when doing a 'BombMove' onto the powerup" $ do
                assertSeries
                    [ "#####"
                    , "# b #"
                    , "# S #"
                    , "#####"
                    ]
                    [player]
                    g
                    explosionResultNoPowerup
                    [ "#####"
                    , "# b #"
                    , "# 1 #"
                    , "#####"
                    ]
                    [ ( [Input player DropBomb, Input player (Move Up)]
                      , [ "#####"
                        , "# 1 #"
                        , "# Q #"
                        , "#####"
                        ]
                      )
                    , ( [Input player DropBomb]
                      , [ "#####"
                        , "# Q #"
                        , "# Q #"
                        , "#####"
                        ]
                      )
                    ]

            it "bomb explosions cascade" $ do
                assertSeries
                    [ "#####"
                    , "# b #"
                    , "# S #"
                    , "#####"
                    ]
                    [player]
                    g
                    explosionResultNoPowerup
                    [ "#####"
                    , "# b #"
                    , "# 1 #"
                    , "#####"
                    ]
                    [ ( [Input player DropBomb, Input player (Move Up)]
                      , [ "#####"
                        , "# 1 #"
                        , "# Q #"
                        , "#####"
                        ]
                      )
                    , ( [Input player DropBomb]
                      , [ "#####"
                        , "# Q #"
                        , "# Q #"
                        , "#####"
                        ]
                      )
                    , ( []
                      , [ "#####"
                        , "# Q #"
                        , "# Q #"
                        , "#####"
                        ]
                      )
                    , ( []
                      , [ "#####"
                        , "#~~~#"
                        , "#~~~#"
                        , "#####"
                        ]
                      )
                    ]

            it "bomb powerup is destructible" $ do
                assertSeries
                    [ "#####"
                    , "# b #"
                    , "# S #"
                    , "#####"
                    ]
                    [player]
                    g
                    explosionResultNoPowerup
                    [ "#####"
                    , "# b #"
                    , "# 1 #"
                    , "#####"
                    ]
                    [ ( [Input player DropBomb]
                      , [ "#####"
                        , "# b #"
                        , "# Q #"
                        , "#####"
                        ]
                      )
                    , ( []
                      , [ "#####"
                        , "# b #"
                        , "# Q #"
                        , "#####"
                        ]
                      )
                    , ( []
                      , [ "#####"
                        , "# b #"
                        , "# Q #"
                        , "#####"
                        ]
                      )
                    , ( []
                      , [ "#####"
                        , "# ~ #"
                        , "#~~~#"
                        , "#####"
                        ]
                      )
                    , ( []
                      , [ "#####"
                        , "#   #"
                        , "#   #"
                        , "#####"
                        ]
                      )
                    ]

assertSeries :: DebugMap -> [Participant] -> StdGen -> (StdGen -> (StdGen, Cell)) -> DebugMap -> [([Input], DebugMap)] -> IO ()
assertSeries debugMap names g erF postSpawn expectations = do
    let
        queueAllInputs :: [Input] -> GameState -> GameState
        queueAllInputs [] s = s
        queueAllInputs (Input p a:ms) s = queueAllInputs ms (queueAction p a s)

        assertOnInitial Nothing = error "invalid map"
        assertOnInitial (Just s) = (show . opaqueify $ s) `shouldBe` intercalate "\n" postSpawn

        go :: [([Input], DebugMap)] -> Maybe GameState -> IO ()
        go _ Nothing = return ()
        go [] _ = return ()
        go ((ms, e):xs) (Just s) = do
            let
                opaqueNewGameState = opaqueify newGameState
                newGameState = tick . queueAllInputs ms $ s

            show opaqueNewGameState `shouldBe` intercalate "\n" e
            go xs (Just newGameState)

        initialGameState = startGame names g erF <$> mapFromDebug debugMap

    assertOnInitial initialGameState
    go expectations initialGameState
