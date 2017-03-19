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
        getPlayers (OpaqueState _ players) = players
        state = startGame [p1, p2] g explosionResult
            <$> mapFromDebug exampleDebugMap

        oState = opaqueify
            <$> tick
            <$> tick
            <$> tick
            <$> tick
            <$> tick
            <$> queueAction p1 (Move Down)
            <$> queueAction p1 DropBomb
            <$> state

    putStrLn ""
    putStrLn . maybe "Invalid Map" show $ oState
    putStrLn ""
    putStrLn . maybe "Invalid Map" (show . getPlayers) $ oState
    putStrLn ""
    print state
