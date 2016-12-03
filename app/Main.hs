module Main where

import Bombastic

exampleDebugMap :: [String]
exampleDebugMap =
    [ "######"
    , "#S.  #"
    , "#.S..#"
    , "#.. S#"
    , "######"
    ]

main :: IO ()
main = do
    let
        state = startGame [mkPlayer "p1", mkPlayer "p2"] <$> mapFromDebug exampleDebugMap

    putStrLn ""
    putStrLn . maybe "Invalid Map" (show . opaqueState) $ state
    putStrLn ""
    putStrLn . maybe "Invalid Map" (show . getPlayers) $ state
