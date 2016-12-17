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
        getPlayers (OpaqueState _ players _ _) = players
        oState = opaqueify <$> state
        state = startGame [PlayerName "p1", PlayerName "p2"]
            <$> mapFromDebug exampleDebugMap

    putStrLn ""
    putStrLn . maybe "Invalid Map" show $ oState
    putStrLn ""
    putStrLn . maybe "Invalid Map" (show . getPlayers) $ oState
    putStrLn ""
    print state
