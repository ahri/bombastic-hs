module Main where

import Bombastic

exampleDebugMap :: [String]
exampleDebugMap =
    [ "######"
    , "#S.S #"
    , "#.S..#"
    , "#.. S#"
    , "######"
    ]

main :: IO ()
main = do
    let
        getPlayers (OpaqueState _ players) = players
        oState = opaqueify <$> state
        state = startGame [mkDebugParticipant 1 "p1", mkDebugParticipant 2 "p2"]
            <$> mapFromDebug exampleDebugMap

    putStrLn ""
    putStrLn . maybe "Invalid Map" show $ oState
    putStrLn ""
    putStrLn . maybe "Invalid Map" (show . getPlayers) $ oState
    putStrLn ""
    print state
