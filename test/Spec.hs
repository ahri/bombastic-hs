{-
 - TODO:
 -  * quit works
 -  * games can be ended
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
    let
        players = [p1, p2, p3]
        p1 = mkDebugParticipant 1 "p1"
        p2 = mkDebugParticipant 2 "p2"
        p3 = mkDebugParticipant 3 "p3"

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
                getCells (OpaqueGameInProgress (OpaqueBoard cells2d) _) = cells2d
                getCells s = error $ "Can't getCells on " ++ show s
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
                opaque = opaqueify . startGame [p1, p2] g explosionResultNoPowerup <$> mapFromDebug
                    [ "SS"
                    , "S "
                    ]
                getPlayers (OpaqueGameInProgress _ ps) = ps
                getPlayers s = error $ "Can't getPlayers on " ++ show s

            (getPlayers <$> opaque) `shouldBe` Just
                [ OpaqueConnectedPlayer (ParticipantName "p1") (Coords 0 0)
                , OpaqueConnectedPlayer (ParticipantName "p2") (Coords 1 0)
                ]


        it "invalid map loads as Nothing" $ do
            let
                opaque = show . opaqueify <$> state
                state = startGame players g explosionResultNoPowerup <$> mapFromDebug invalidMap

            opaque `shouldBe` Nothing

    describe "Tick" $ do
        it "no-op returns same state" $ do
            let
                state = startGame players g explosionResultNoPowerup <$> mapFromDebug validMap

            tick <$> state `shouldBe` state

        context "game ending" $ do
            let
                moveMap =
                    [ "#####"
                    , "#SSS#"
                    , "#####"
                    ]

            it "a player quits" $ do
                assertSeries
                    moveMap
                    players
                    g
                    explosionResultNoPowerup
                    [ "#####"
                    , "#123#"
                    , "#####"
                    ]
                    [ ( [Input p1 QuitGame]
                      , [ "#####"
                        , "# 23#"
                        , "#####"
                        ]
                      )
                    ]

            it "all players quit" $ do
                assertSeries
                    moveMap
                    [p1, p2, p3]
                    g
                    explosionResultNoPowerup
                    [ "#####"
                    , "#123#"
                    , "#####"
                    ]
                    [ ( [Input p1 QuitGame, Input p2 QuitGame, Input p3 QuitGame]
                      , [ "#####"
                        , "#   #"
                        , "#####"
                        ]
                      )
                    , ( []
                      , ["Draw"]
                      )
                    ]

            it "one player remains" $ do
                assertSeries
                    moveMap
                    [p1, p2, p3]
                    g
                    explosionResultNoPowerup
                    [ "#####"
                    , "#123#"
                    , "#####"
                    ]
                    [ ( [Input p1 QuitGame, Input p3 QuitGame]
                      , [ "#####"
                        , "# 2 #"
                        , "#####"
                        ]
                      )
                    , ( []
                      , ["Winner: p2"]
                      )
                    ]

        context "movement" $ do
            let
                initialIndestructibleBlockGameState = startGame players g explosionResultNoPowerup <$>
                    mapFromDebug indestructibleBlockMap

                initialDestructibleBlockGameState = startGame players g explosionResultNoPowerup <$>
                    mapFromDebug destructibleBlockMap

                moveMap =
                    [ "#####"
                    , "#   #"
                    , "# S #"
                    , "# S #"
                    , "#####"
                    ]

                indestructibleBlockMap =
                    [ "#####"
                    , "#S#S#"
                    , "#####"
                    ]

                destructibleBlockMap =
                    [ "+++++"
                    , "+S+S+"
                    , "+++++"
                    ]

                doesntMoveWhenTicked action state =
                    (opaqueify . tick . queueAction p1 action <$> state) `shouldBe` (opaqueify <$> state)

            it "up" $ do
                assertSeries
                    moveMap
                    players
                    g
                    explosionResultNoPowerup
                    [ "#####"
                    , "#   #"
                    , "# 1 #"
                    , "# 2 #"
                    , "#####"
                    ]
                    [ ( [Input p1 (Move Up)]
                      , [ "#####"
                        , "# 1 #"
                        , "#   #"
                        , "# 2 #"
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
                    players
                    g
                    explosionResultNoPowerup
                    [ "#####"
                    , "#   #"
                    , "# 1 #"
                    , "# 2 #"
                    , "#####"
                    ]
                    [ ( [Input p1 (Move Down)]
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
                    players
                    g
                    explosionResultNoPowerup
                    [ "#####"
                    , "#   #"
                    , "# 1 #"
                    , "# 2 #"
                    , "#####"
                    ]
                    [ ( [Input p1 (Move Bombastic.Left)]
                      , [ "#####"
                        , "#   #"
                        , "#1  #"
                        , "# 2 #"
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
                    players
                    g
                    explosionResultNoPowerup
                    [ "#####"
                    , "#   #"
                    , "# 1 #"
                    , "# 2 #"
                    , "#####"
                    ]
                    [ ( [Input p1 (Move Bombastic.Right)]
                      , [ "#####"
                        , "#   #"
                        , "#  1#"
                        , "# 2 #"
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
                    , "#SS #"
                    , "#####"
                    ]
                    players
                    g
                    explosionResultNoPowerup
                    [ "#####"
                    , "#12 #"
                    , "#####"
                    ]
                    [ ( [Input p1 (Move Bombastic.Right)]
                      , [ "#####"
                        , "# 1 #"
                        , "#####"
                        ]
                      )
                    , ( []
                      , [ "#####"
                        , "# 21#"
                        , "#####"
                        ]
                      )
                    ]

            it "last direction wins" $ do
                assertSeries
                    moveMap
                    players
                    g
                    explosionResultNoPowerup
                    [ "#####"
                    , "#   #"
                    , "# 1 #"
                    , "# 2 #"
                    , "#####"
                    ]
                    [ ( [Input p1 (Move Bombastic.Left), Input p1 (Move Bombastic.Right)]
                      , [ "#####"
                        , "#   #"
                        , "#  1#"
                        , "# 2 #"
                        , "#####"
                        ]
                      )
                    ]
                

        context "bombing" $ do
            let
                moveMap =
                    [ "#####"
                    , "#   #"
                    , "# S #"
                    , "# S #"
                    , "#####"
                    ]

            it "a bomb is dropped" $ do
                assertSeries
                    moveMap
                    players
                    g
                    explosionResultNoPowerup
                    [ "#####"
                    , "#   #"
                    , "# 1 #"
                    , "# 2 #"
                    , "#####"
                    ]
                    [ ( [Input p1 DropBomb]
                      , [ "#####"
                        , "#   #"
                        , "# Q #"
                        , "# 2 #"
                        , "#####"
                        ]
                      )
                    ]

            it "a bomb is dropped and movement occurs" $ do
                assertSeries
                    moveMap
                    players
                    g
                    explosionResultNoPowerup
                    [ "#####"
                    , "#   #"
                    , "# 1 #"
                    , "# 2 #"
                    , "#####"
                    ]
                    [ ( [ Input p1 DropBomb
                        , Input p1 (Move Bombastic.Right)
                        ]
                      , [ "#####"
                        , "#   #"
                        , "# Q1#"
                        , "# 2 #"
                        , "#####"
                        ]
                      )
                    ]

            it "movement occurs and a bomb is dropped" $ do
                assertSeries
                    moveMap
                    players
                    g
                    explosionResultNoPowerup
                    [ "#####"
                    , "#   #"
                    , "# 1 #"
                    , "# 2 #"
                    , "#####"
                    ]
                    [ ( [ Input p1 (Move Bombastic.Right)
                        , Input p1 DropBomb
                        ]
                      , [ "#####"
                        , "#   #"
                        , "# Q1#"
                        , "# 2 #"
                        , "#####"
                        ]
                      )
                    ]

            it "movement continues without bombs dropping" $ do
                assertSeries
                    [ "#####"
                    , "#SS #"
                    , "#####"
                    ]
                    players
                    g
                    explosionResultNoPowerup
                    [ "#####"
                    , "#12 #"
                    , "#####"
                    ]
                    [ ( [ Input p1 (Move Bombastic.Right)
                        , Input p1 DropBomb
                        ]
                      , [ "#####"
                        , "#Q1 #"
                        , "#####"
                        ]
                      )
                    , ( []
                      , [ "#####"
                        , "#Q21#"
                        , "#####"
                        ]
                      )
                    ]

            it "change of mind in direction still drops bomb" $ do
                assertSeries
                    [ "#####"
                    , "# SS#"
                    , "#####"
                    ]
                    players
                    g
                    explosionResultNoPowerup
                    [ "#####"
                    , "# 12#"
                    , "#####"
                    ]
                    [ ( [ Input p1 (Move Bombastic.Right)
                        , Input p1 DropBomb
                        , Input p1 (Move Bombastic.Left)
                        ]
                      , [ "#####"
                        , "#1Q2#"
                        , "#####"
                        ]
                      )
                    ]

            it "repeated bomb action doesn't change anything" $ do
                assertSeries
                    [ "#####"
                    , "# SS#"
                    , "#####"
                    ]
                    players
                    g
                    explosionResultNoPowerup
                    [ "#####"
                    , "# 12#"
                    , "#####"
                    ]
                    [ ( [ Input p1 (Move Bombastic.Right)
                        , Input p1 DropBomb
                        , Input p1 DropBomb
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
                    , "#S S#"
                    , "#####"
                    ]
                    players
                    g
                    explosionResultNoPowerup
                    [ "#####"
                    , "#1 2#"
                    , "#####"
                    ]
                    [ ( [ Input p1 (Move Bombastic.Right)
                        , Input p1 DropBomb
                        ]
                      , [ "#####"
                        , "#Q12#"
                        , "#####"
                        ]
                      )
                    , ( [Input p1 (Move Bombastic.Left)]
                      , [ "#####"
                        , "#Q12#"
                        , "#####"
                        ]
                      )
                    ]

        context "flame" $ do
            it "basic bomb detonates after 3 ticks" $ do
                assertSeries
                    [ "#######"
                    , "#     #"
                    , "#     #"
                    , "#  S  #"
                    , "#     #"
                    , "#    S#"
                    , "#######"
                    ]
                    players
                    g
                    explosionResultNoPowerup
                    [ "#######"
                    , "#     #"
                    , "#     #"
                    , "#  1  #"
                    , "#     #"
                    , "#    2#"
                    , "#######"
                    ]
                    [ ( [Input p1 DropBomb, Input p1 (Move Up)]
                      , [ "#######"
                        , "#     #"
                        , "#  1  #"
                        , "#  Q  #"
                        , "#     #"
                        , "#    2#"
                        , "#######"
                        ]
                      )
                    , ( []
                      , [ "#######"
                        , "#  1  #"
                        , "#     #"
                        , "#  Q  #"
                        , "#     #"
                        , "#    2#"
                        , "#######"
                        ]
                      )
                    , ( []
                      , [ "#######"
                        , "#  1  #"
                        , "#     #"
                        , "#  Q  #"
                        , "#     #"
                        , "#    2#"
                        , "#######"
                        ]
                      )
                    , ( []
                      , [ "#######"
                        , "#  1  #"
                        , "#  ~  #"
                        , "# ~~~ #"
                        , "#  ~  #"
                        , "#    2#"
                        , "#######"
                        ]
                      )
                    , ( []
                      , [ "#######"
                        , "#  1  #"
                        , "#     #"
                        , "#     #"
                        , "#     #"
                        , "#    2#"
                        , "#######"
                        ]
                      )
                    ]

            it "bomb explosion kills player" $ do
                assertSeries
                    [ "######"
                    , "#S #S#"
                    , "######"
                    ]
                    players
                    g
                    explosionResultNoPowerup
                    [ "######"
                    , "#1 #2#"
                    , "######"
                    ]
                    [ ( [Input p1 DropBomb]
                      , [ "######"
                        , "#Q #2#"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "#Q #2#"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "#Q #2#"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "#~~#2#"
                        , "######"
                        ]
                      )
                    , ( []
                      , ["Winner: p2"]
                      )
                    ]

            it "bomb flame kills player" $ do
                assertSeries
                    [ "######"
                    , "#S #S#"
                    , "######"
                    ]
                    players
                    g
                    explosionResultNoPowerup
                    [ "######"
                    , "#1 #2#"
                    , "######"
                    ]
                    [ ( [Input p1 DropBomb, Input p1 (Move Bombastic.Right)]
                      , [ "######"
                        , "#Q1#2#"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "#Q1#2#"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "#Q1#2#"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "#~~#2#"
                        , "######"
                        ]
                      )
                    , ( []
                      , ["Winner: p2"]
                      )
                    ]

            it "bomb destroys destructible block but not indestructible block" $ do
                assertSeries
                    [ "######"
                    , "#S+#S#"
                    , "######"
                    ]
                    players
                    g
                    explosionResultNoPowerup
                    [ "######"
                    , "#1+#2#"
                    , "######"
                    ]
                    [ ( [Input p1 DropBomb]
                      , [ "######"
                        , "#Q+#2#"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "#Q+#2#"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "#Q+#2#"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "#~~#2#"
                        , "######"
                        ]
                      )
                    , ( []
                      , ["Winner: p2"]
                      )
                    ]

            it "flame is not inhibited by a powerup, but destructible blocks do inhibit it" $ do
                assertSeries
                    [ "########"
                    , "#S++ #S#"
                    , "#    ###"
                    , "######"
                    ]
                    players
                    g
                    (\g' -> (g', Powerup FlamePowerup))
                    [ "########"
                    , "#1++ #2#"
                    , "#    ###"
                    , "######"
                    ]
                    [ ( [Input p1 DropBomb, Input p1 (Move Down)]
                      , [ "########"
                        , "#Q++ #2#"
                        , "#1   ###"
                        , "######"
                        ]
                      )
                    , ( [Input p1 (Move Bombastic.Right)]
                      , [ "########"
                        , "#Q++ #2#"
                        , "# 1  ###"
                        , "######"
                        ]
                      )
                    , ( [Input p1 (Move Down)]
                      , [ "########"
                        , "#Q++ #2#"
                        , "# 1  ###"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "########"
                        , "#~~+ #2#"
                        , "#~1  ###"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "########"
                        , "# f+ #2#"
                        , "# 1  ###"
                        , "######"
                        ]
                      )
                    , ( [Input p1 (Move Up)]
                      , [ "########"
                        , "# 1+ #2#"
                        , "#    ###"
                        , "######"
                        ]
                      )
                    , ( [Input p1 DropBomb, Input p1 (Move Down)]
                      , [ "########"
                        , "# Q+ #2#"
                        , "# 1  ###"
                        , "######"
                        ]
                      )
                    , ( [Input p1 (Move Bombastic.Right)]
                      , [ "########"
                        , "# Q+ #2#"
                        , "#  1 ###"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "########"
                        , "# Q+ #2#"
                        , "#   1###"
                        , "######"
                        ]
                      )
                    , ( [Input p1 (Move Up)]
                      , [ "########"
                        , "#~~~1#2#"
                        , "# ~  ###"
                        , "######"
                        ]
                      )
                    , ( [Input p1 DropBomb]
                      , [ "########"
                        , "#  fQ#2#"
                        , "#    ###"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "########"
                        , "#  fQ#2#"
                        , "#    ###"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "########"
                        , "#  fQ#2#"
                        , "#    ###"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "########"
                        , "# ~~~#2#"
                        , "#   ~###"
                        , "######"
                        ]
                      )
                    , ( []
                      , ["Winner: p2"]
                      )
                    ]

            it "flames cross over" $ do
                assertSeries
                    [ "#######"
                    , "#Sbbf #"
                    , "#     #"
                    , "#S    #"
                    , "#######"
                    ]
                    players
                    g
                    explosionResultNoPowerup
                    [ "#######"
                    , "#1bbf #"
                    , "#     #"
                    , "#2    #"
                    , "#######"
                    ]
                    [ ( [Input p1 (Move Bombastic.Right)]
                      , [ "#######"
                        , "# 1bf #"
                        , "#     #"
                        , "#2    #"
                        , "#######"
                        ]
                      )
                    , ( []
                      , [ "#######"
                        , "#  1f #"
                        , "#     #"
                        , "#2    #"
                        , "#######"
                        ]
                      )
                    , ( []
                      , [ "#######"
                        , "#   1 #"
                        , "#     #"
                        , "#2    #"
                        , "#######"
                        ]
                      )
                    , ( [Input p1 DropBomb]
                      , [ "#######"
                        , "#   Q1#"
                        , "#     #"
                        , "#2    #"
                        , "#######"
                        ]
                      )
                    , ( [Input p1 DropBomb, Input p1 (Move Down)]
                      , [ "#######"
                        , "#   QQ#"
                        , "#    1#"
                        , "#2    #"
                        , "#######"
                        ]
                      )
                    , ( [Input p1 DropBomb]
                      , [ "#######"
                        , "#   QQ#"
                        , "#    Q#"
                        , "#2   1#"
                        , "#######"
                        ]
                      )
                    , ( [Input p1 DropBomb]
                      , [ "#######"
                        , "# ~~~~#"
                        , "#  ~~~#"
                        , "#2  ~~#"
                        , "#######"
                        ]
                      )
                    ]

            it "flames cross over pending powerup drops" $ do
                assertSeries
                    [ "#######"
                    , "#Sbbf #"
                    , "#   + #"
                    , "#S    #"
                    , "#######"
                    ]
                    players
                    g
                    (\g' -> (g', Powerup FlamePowerup))
                    [ "#######"
                    , "#1bbf #"
                    , "#   + #"
                    , "#2    #"
                    , "#######"
                    ]
                    [ ( [Input p1 (Move Bombastic.Right)]
                      , [ "#######"
                        , "# 1bf #"
                        , "#   + #"
                        , "#2    #"
                        , "#######"
                        ]
                      )
                    , ( []
                      , [ "#######"
                        , "#  1f #"
                        , "#   + #"
                        , "#2    #"
                        , "#######"
                        ]
                      )
                    , ( []
                      , [ "#######"
                        , "#   1 #"
                        , "#   + #"
                        , "#2    #"
                        , "#######"
                        ]
                      )
                    , ( [Input p1 DropBomb]
                      , [ "#######"
                        , "#   Q1#"
                        , "#   + #"
                        , "#2    #"
                        , "#######"
                        ]
                      )
                    , ( [Input p1 DropBomb, Input p1 (Move Down)]
                      , [ "#######"
                        , "#   QQ#"
                        , "#   +1#"
                        , "#2    #"
                        , "#######"
                        ]
                      )
                    , ( [Input p1 DropBomb]
                      , [ "#######"
                        , "#   QQ#"
                        , "#   +Q#"
                        , "#2   1#"
                        , "#######"
                        ]
                      )
                    , ( []
                      , [ "#######"
                        , "# ~~~~#"
                        , "#  ~~~#"
                        , "#2   ~#"
                        , "#######"
                        ]
                      )
                    , ( []
                      , ["Winner: p2"]
                      )
                    ]

            it "hammering drop bomb makes no difference if player dies" $ do
                assertSeries
                    [ "######"
                    , "#S #S#"
                    , "######"
                    ]
                    players
                    g
                    explosionResultNoPowerup
                    [ "######"
                    , "#1 #2#"
                    , "######"
                    ]
                    [ ( [Input p1 DropBomb, Input p1 (Move Bombastic.Right)]
                      , [ "######"
                        , "#Q1#2#"
                        , "######"
                        ]
                      )
                    , ( [Input p1 DropBomb]
                      , [ "######"
                        , "#Q1#2#"
                        , "######"
                        ]
                      )
                    , ( [Input p1 DropBomb]
                      , [ "######"
                        , "#Q1#2#"
                        , "######"
                        ]
                      )
                    , ( [Input p1 DropBomb]
                      , [ "######"
                        , "#~~#2#"
                        , "######"
                        ]
                      )
                    , ( [Input p1 DropBomb]
                      , ["Winner: p2"]
                      )
                    ]

        context "powerups" $ do
            let

            it "player cannot drop more than one bomb" $ do
                assertSeries
                    [ "#####"
                    , "#S S#"
                    , "#####"
                    ]
                    players
                    g
                    explosionResultNoPowerup
                    [ "#####"
                    , "#1 2#"
                    , "#####"
                    ]
                    [ ( [ Input p1 (Move Bombastic.Right)
                        , Input p1 DropBomb
                        ]
                      , [ "#####"
                        , "#Q12#"
                        , "#####"
                        ]
                      )
                    , ( [Input p1 DropBomb]
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
                    , "#+  S#"
                    , "######"
                    ]
                    players
                    g
                    explosionResultNoPowerup
                    [ "######"
                    , "#1 + #"
                    , "# +  #"
                    , "#+  2#"
                    , "######"
                    ]
                    [ ( [Input p1 (Move Down)]
                      , [ "######"
                        , "#  + #"
                        , "#1+  #"
                        , "#+  2#"
                        , "######"
                        ]
                      )
                    , ( [Input p1 DropBomb, Input p1 (Move Up)]
                      , [ "######"
                        , "#1 + #"
                        , "#Q+  #"
                        , "#+  2#"
                        , "######"
                        ]
                      )
                    , ( [Input p1 (Move Bombastic.Right)]
                      , [ "######"
                        , "# 1+ #"
                        , "#Q+  #"
                        , "#+  2#"
                        , "######"
                        ]
                      )
                    , ( [Input p1 DropBomb]
                      , [ "######"
                        , "# 1+ #"
                        , "#Q+  #"
                        , "#+  2#"
                        , "######"
                        ]
                      )
                    , ( [Input p1 DropBomb]
                      , [ "######"
                        , "#~Q+ #"
                        , "#~~  #"
                        , "#~  2#"
                        , "######"
                        ]
                      )
                    ]

            it "can drop flame powerup from tests" $ do
                assertSeries
                    [ "######"
                    , "#S+SS#"
                    , "######"
                    ]
                    players
                    g
                    (\g' -> (g', Powerup FlamePowerup))
                    [ "######"
                    , "#1+23#"
                    , "######"
                    ]
                    [ ( [Input p1 DropBomb]
                      , [ "######"
                        , "#Q+23#"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "#Q+23#"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "#Q+23#"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "#~~23#"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "# f23#"
                        , "######"
                        ]
                      )
                    ]

            it "flame powerup increases flame length" $ do
                assertSeries
                    [ "#####"
                    , "#Sf #"
                    , "# S #"
                    , "#####"
                    ]
                    players
                    g
                    explosionResultNoPowerup
                    [ "#####"
                    , "#1f #"
                    , "# 2 #"
                    , "#####"
                    ]
                    [ ( [Input p1 (Move Bombastic.Right)]
                      , [ "#####"
                        , "# 1 #"
                        , "# 2 #"
                        , "#####"
                        ]
                      )
                    , ( [Input p1 (Move Bombastic.Right)]
                      , [ "#####"
                        , "#  1#"
                        , "# 2 #"
                        , "#####"
                        ]
                      )
                    , ( [Input p1 DropBomb, Input p1 (Move Down)]
                      , [ "#####"
                        , "#  Q#"
                        , "# 21#"
                        , "#####"
                        ]
                      )
                    , ( [Input p1 (Move Bombastic.Left)]
                      , [ "#####"
                        , "#  Q#"
                        , "# 1 #"
                        , "#####"
                        ]
                      )
                    , ( []
                      , [ "#####"
                        , "#  Q#"
                        , "#12 #"
                        , "#####"
                        ]
                      )
                    , ( []
                      , [ "#####"
                        , "#~~~#"
                        , "#12~#"
                        , "#####"
                        ]
                      )
                    ]

            it "flame powerup increases flame length, when doing a 'BombMove' onto the powerup" $ do
                assertSeries
                    [ "######"
                    , "#  f #"
                    , "#  SS#"
                    , "######"
                    ]
                    players
                    g
                    explosionResultNoPowerup
                    [ "######"
                    , "#  f #"
                    , "#  12#"
                    , "######"
                    ]
                    [ ( [ Input p2 (Move Bombastic.Left)
                        , Input p1 DropBomb
                        , Input p1 (Move Up)
                        ]
                      , [ "######"
                        , "#  1 #"
                        , "#  Q #"
                        , "######"
                        ]
                      )
                    , ( [Input p1 (Move Bombastic.Right)]
                      , [ "######"
                        , "#   1#"
                        , "# 2Q #"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "#   1#"
                        , "#2 Q #"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "#  ~1#"
                        , "#2~~~#"
                        , "######"
                        ]
                      )
                    , ( [Input p1 DropBomb, Input p1 (Move Down)]
                      , [ "######"
                        , "#   Q#"
                        , "#2  1#"
                        , "######"
                        ]
                      )
                    , ( [Input p1 (Move Bombastic.Left)]
                      , [ "######"
                        , "#   Q#"
                        , "#2 1 #"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "#   Q#"
                        , "#21  #"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "# ~~~#"
                        , "#1  ~#"
                        , "######"
                        ]
                      )
                    ]

            it "flame powerup is destructible" $ do
                assertSeries
                    [ "#####"
                    , "# f #"
                    , "# SS#"
                    , "#####"
                    ]
                    players
                    g
                    explosionResultNoPowerup
                    [ "#####"
                    , "# f #"
                    , "# 12#"
                    , "#####"
                    ]
                    [ ( [Input p1 DropBomb, Input p1 (Move Bombastic.Left)]
                      , [ "#####"
                        , "# f #"
                        , "#1Q2#"
                        , "#####"
                        ]
                      )
                    , ( [Input p1 (Move Up), Input p2 (Move Up)]
                      , [ "#####"
                        , "#1f2#"
                        , "# Q #"
                        , "#####"
                        ]
                      )
                    , ( []
                      , [ "#####"
                        , "#1f2#"
                        , "# Q #"
                        , "#####"
                        ]
                      )
                    , ( []
                      , [ "#####"
                        , "#1~2#"
                        , "#~~~#"
                        , "#####"
                        ]
                      )
                    , ( []
                      , [ "#####"
                        , "#1 2#"
                        , "#   #"
                        , "#####"
                        ]
                      )
                    ]

            it "can drop bomb powerup from tests" $ do
                assertSeries
                    [ "######"
                    , "#S+SS#"
                    , "######"
                    ]
                    players
                    g
                    (\g' -> (g', Powerup BombPowerup))
                    [ "######"
                    , "#1+23#"
                    , "######"
                    ]
                    [ ( [Input p1 DropBomb]
                      , [ "######"
                        , "#Q+23#"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "#Q+23#"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "#Q+23#"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "#~~23#"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "# b23#"
                        , "######"
                        ]
                      )
                    ]

            it "bomb powerup increases number of bombs you can drop" $ do
                assertSeries
                    [ "#####"
                    , "#Sb #"
                    , "# S #"
                    , "#####"
                    ]
                    players
                    g
                    explosionResultNoPowerup
                    [ "#####"
                    , "#1b #"
                    , "# 2 #"
                    , "#####"
                    ]
                    [ ( [Input p1 (Move Bombastic.Right)]
                      , [ "#####"
                        , "# 1 #"
                        , "# 2 #"
                        , "#####"
                        ]
                      )
                    , ( [Input p1 (Move Bombastic.Right)]
                      , [ "#####"
                        , "#  1#"
                        , "# 2 #"
                        , "#####"
                        ]
                      )
                    , ( [Input p1 DropBomb, Input p1 (Move Down)]
                      , [ "#####"
                        , "#  Q#"
                        , "# 21#"
                        , "#####"
                        ]
                      )
                    , ( [Input p1 DropBomb]
                      , [ "#####"
                        , "#  Q#"
                        , "# 2Q#"
                        , "#####"
                        ]
                      )
                    ]

            it "bomb powerup increases number of bombs you can drop, when doing a 'BombMove' onto the powerup" $ do
                assertSeries
                    [ "#####"
                    , "# b #"
                    , "# SS#"
                    , "#####"
                    ]
                    players
                    g
                    explosionResultNoPowerup
                    [ "#####"
                    , "# b #"
                    , "# 12#"
                    , "#####"
                    ]
                    [ ( [Input p1 DropBomb, Input p1 (Move Up)]
                      , [ "#####"
                        , "# 1 #"
                        , "# Q2#"
                        , "#####"
                        ]
                      )
                    , ( [Input p1 DropBomb]
                      , [ "#####"
                        , "# Q #"
                        , "# Q2#"
                        , "#####"
                        ]
                      )
                    ]

            it "bomb explosions cascade" $ do
                assertSeries
                    [ "#####"
                    , "# b #"
                    , "# SS#"
                    , "#####"
                    ]
                    players
                    g
                    explosionResultNoPowerup
                    [ "#####"
                    , "# b #"
                    , "# 12#"
                    , "#####"
                    ]
                    [ ( [Input p1 DropBomb, Input p1 (Move Up)]
                      , [ "#####"
                        , "# 1 #"
                        , "# Q2#"
                        , "#####"
                        ]
                      )
                    , ( [Input p1 DropBomb]
                      , [ "#####"
                        , "# Q #"
                        , "# Q2#"
                        , "#####"
                        ]
                      )
                    , ( []
                      , [ "#####"
                        , "# Q #"
                        , "# Q2#"
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
                    , ( []
                      , ["Draw"]
                      )
                    ]

            it "bomb powerup is destructible" $ do
                assertSeries
                    [ "######"
                    , "# b  #"
                    , "# S S#"
                    , "######"
                    ]
                    players
                    g
                    explosionResultNoPowerup
                    [ "######"
                    , "# b  #"
                    , "# 1 2#"
                    , "######"
                    ]
                    [ ( [Input p1 DropBomb, Input p1 (Move Bombastic.Right)]
                      , [ "######"
                        , "# b  #"
                        , "# Q12#"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "# b  #"
                        , "# Q 1#"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "# b  #"
                        , "# Q 1#"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "# ~  #"
                        , "#~~~1#"
                        , "######"
                        ]
                      )
                    , ( []
                      , [ "######"
                        , "#    #"
                        , "#   1#"
                        , "######"
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
