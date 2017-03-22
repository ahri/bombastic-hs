module Main where

import System.Random
import Bombastic

exampleDebugMap :: [String]
exampleDebugMap =
    [ "##############"
    , "#S+          #"
    , "# +          #"
    , "# +          #"
    , "##############"
    ]

main :: IO ()
main = do
    g <- getStdGen

    let
        p1 = mkDebugParticipant 1 "p1"
        p2 = mkDebugParticipant 1 "p2"
        getPlayers (OpaqueGameState _ players) = players
        state = startGame [p1, p2] g explosionResult
            <$> mapFromDebug exampleDebugMap

        oGameState = opaqueify
            <$> tick
            <$> tick
            <$> tick
            <$> tick
            <$> tick
            <$> queueAction p1 (Move Down)
            <$> queueAction p1 DropBomb
            <$> state

    putStrLn ""
    putStrLn . maybe "Invalid Map" show $ oGameState
    putStrLn ""
    putStrLn . maybe "Invalid Map" (show . getPlayers) $ oGameState
    putStrLn ""
    print state
