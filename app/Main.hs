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
    putStrLn ""
    putStrLn . maybe "Invalid Map" (show . opaqueState) $ state
    putStrLn ""
    putStrLn . maybe "Invalid Map" (show . getPlayers) $ state
    where
        state = startGame [mkPlayer "p1", mkPlayer "p2"] <$> mapFromDebug exampleDebugMap
