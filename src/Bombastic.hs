module Bombastic
    ( mapFromDebug
    , mkPlayer
    , startGame
    , tick

    , queueAction
    , Action (..)

    , opaqueify
    , OpaqueState (..)
    , Coords (..)
    , Board (..)
    , Cell (..)
    , OpaquePlayer (..)
    , OpaqueStuff (..)
    ) where

import Data.List

-- Storage

newtype Map = Map [[Tile]] deriving (Eq, Show)

data Tile
    = EmptyTile
    | IndestructibleTile
    | DestructibleTile
    | PlayerStartPosition
    deriving (Eq, Show)


-- State

data State = State
    Board
    [PlayerSlot]
    [Stuff]
    [Bomb]
    deriving (Eq, Show)

newtype Board = Board [[Cell]] deriving (Eq, Show)

data PlayerSlot
    = DisconnectedPlayer
    | ConnectedPlayer
        Action
        Player
        Coords
    deriving (Eq, Show)

data Stuff
    = DestructibleBlock Coords
    | Flame Coords Player
    | FlamePowerup Coords
    | BombPowerup Coords
    deriving (Eq, Show)

newtype Coords = Coords (Int, Int) deriving (Eq, Show)

data Action
    = NoAction
    | MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight
    | DropBomb
    | QuitGame
    deriving (Eq, Show)

data Cell
    = EmptyCell
    | IndestructibleBlock
    deriving (Eq, Show)

data Player = Player
    PlayerName
    Score
    BombCount
    FlameCount
    deriving (Eq, Show)

newtype PlayerName = PlayerName String deriving (Eq, Show)

mkPlayer :: String -> Player
mkPlayer name = Player (PlayerName name) (Score 0) (BombCount 1) (FlameCount 1)

data Bomb = Bomb
    Coords
    BombTicksLeft
    Player
    deriving (Eq, Show)

newtype Score = Score Integer deriving (Eq, Show)
newtype BombCount = BombCount Integer deriving (Eq, Show)
newtype FlameCount = FlameCount Integer deriving (Eq, Show)
newtype BombTicksLeft = BombTicksLeft Integer deriving (Eq, Show)


-- Transmission

data OpaqueState = OpaqueState
    Board
    [OpaquePlayer]
    [OpaqueStuff]
    [OpaqueBomb]
    deriving (Eq)

instance Show OpaqueState where
    show (OpaqueState board players stuffs bombs) = stringify board
        where
            stringify (Board cells2d) = intercalate "\n"
                $ stringify2d (Coords (0, 0)) cells2d

            stringify2d :: Coords -> [[Cell]] -> [String]
            stringify2d _ [] = []
            stringify2d coords@(Coords (x, y)) (r:rs)
                = stringify1d coords r
                : stringify2d (Coords (x, y + 1)) rs

            stringify1d :: Coords -> [Cell] -> String
            stringify1d _ [] = []
            stringify1d coords@(Coords (x, y)) (c:cs)
                = stringifyCell coords c
                : stringify1d (Coords (x + 1, y)) cs

            stringifyCell :: Coords -> Cell -> Char
            stringifyCell _ IndestructibleBlock = '#'
            stringifyCell coords EmptyCell =
                firstJustOrDefault coords ' '
                    [ chrFromStuffs stuffs
                    , chrFromBombs bombs
                    , chrFromPlayers 0 players
                    ]
                where
                    firstJustOrDefault :: a -> b -> [a -> Maybe b] -> b
                    firstJustOrDefault _ def [] = def
                    firstJustOrDefault input def (f:fs) = case f input of
                        (Just o) ->  o
                        Nothing  -> firstJustOrDefault input def fs

                    -- TODO: map of (Coords, OpaqueStuff) would be prettier
                    chrFromStuffs :: [OpaqueStuff] -> Coords -> Maybe Char
                    chrFromStuffs [] _ = Nothing
                    chrFromStuffs (OpaqueDestructibleBlock (Coords (x', y')):ss) coords'@(Coords (x, y))
                        | x == x' && y == y' = Just '.'
                        | otherwise = chrFromStuffs ss coords'
                    chrFromStuffs (OpaqueFlame (Coords (x', y')):ss) coords'@(Coords (x, y))
                        | x == x' && y == y' = Just '~'
                        | otherwise = chrFromStuffs ss coords'
                    chrFromStuffs (OpaqueFlamePowerup (Coords (x', y')):ss) coords'@(Coords (x, y))
                        | x == x' && y == y' = Just 'f'
                        | otherwise = chrFromStuffs ss coords'
                    chrFromStuffs (OpaqueBombPowerup (Coords (x', y')):ss) coords'@(Coords (x, y))
                        | x == x' && y == y' = Just 'b'
                        | otherwise = chrFromStuffs ss coords'

                    chrFromBombs :: [OpaqueBomb] -> Coords -> Maybe Char
                    chrFromBombs [] _ = Nothing
                    chrFromBombs (OpaqueBomb (Coords (x', y')):bs) coords'@(Coords (x, y))
                        | x == x' && y == y' = Just 'x'
                        | otherwise = chrFromBombs bs coords'

                    chrFromPlayers :: Int -> [OpaquePlayer] -> Coords -> Maybe Char
                    chrFromPlayers _ [] _ = Nothing
                    chrFromPlayers i (OpaqueDisconnectedPlayer:ps) coords' = chrFromPlayers (i + 1) ps coords'
                    chrFromPlayers i (OpaqueConnectedPlayer (Coords (x', y')):ps) coords'@(Coords (x, y))
                        | x == x' && y == y' = Just (head (show i))
                        | otherwise = chrFromPlayers (i + 1) ps coords'

data OpaquePlayer
    = OpaqueDisconnectedPlayer
    | OpaqueConnectedPlayer Coords
    deriving (Eq, Show)

data OpaqueStuff
    = OpaqueDestructibleBlock Coords
    | OpaqueFlame Coords
    | OpaqueFlamePowerup Coords
    | OpaqueBombPowerup Coords
    deriving (Eq, Show)

data OpaqueBomb = OpaqueBomb Coords deriving (Eq, Show)

opaqueify :: State -> OpaqueState
opaqueify (State board playerSlots stuffs bombs) = OpaqueState
    board
    (opaqueifyPlayerSlot <$> playerSlots)
    (opaqueifyStuff      <$> stuffs)
    (opaqueifyBomb       <$> bombs)
    where
        opaqueifyPlayerSlot :: PlayerSlot -> OpaquePlayer
        opaqueifyPlayerSlot DisconnectedPlayer      = OpaqueDisconnectedPlayer
        opaqueifyPlayerSlot (ConnectedPlayer _ _ c) = OpaqueConnectedPlayer c

        opaqueifyStuff :: Stuff -> OpaqueStuff
        opaqueifyStuff (DestructibleBlock c) = OpaqueDestructibleBlock c
        opaqueifyStuff (Flame c _)           = OpaqueFlame c
        opaqueifyStuff (FlamePowerup c)      = OpaqueFlamePowerup c
        opaqueifyStuff (BombPowerup c)       = OpaqueBombPowerup c

        opaqueifyBomb :: Bomb -> OpaqueBomb
        opaqueifyBomb (Bomb c _ _) = OpaqueBomb c


-- Map loading

charToTile :: Char -> Maybe Tile
charToTile '#' = Just IndestructibleTile
charToTile '.' = Just DestructibleTile
charToTile ' ' = Just EmptyTile
charToTile 'S' = Just PlayerStartPosition
charToTile  _  = Nothing

mapFromDebug :: [String] -> Maybe Map
mapFromDebug = fmap Map . sequence . fmap (sequence . fmap charToTile)


-- Game initialization

startGame :: [Player] -> Map -> State
startGame players (Map tiles2d) = State board playerSlots stuffs []
    where
        board = Board . el1_3 $ converted
        playerSlots = el2_3 converted
        stuffs = el3_3 converted

        el1_3 (el, _, _) = el
        el2_3 (_, el, _) = el
        el3_3 (_, _, el) = el

        el1_4 (el, _, _, _) = el
        el2_4 (_, el, _, _) = el
        el3_4 (_, _, el, _) = el
        el4_4 (_, _, _, el) = el

        converted = convert (Coords (0, 0)) players tiles2d

        convert :: Coords -> [Player] -> [[Tile]]
                -> ([[Cell]], [PlayerSlot], [Stuff])
        convert _ _ [] = ([], [], [])
        convert coords@(Coords (x, y)) ps (tr:trs) =
            ( el1_4 convertedRow  : el1_3 recurse
            , el2_4 convertedRow ++ el2_3 recurse
            , el3_4 convertedRow ++ el3_3 recurse
            )
            where
                recurse = convert (Coords (x, y + 1)) (el4_4 convertedRow) trs
                convertedRow = convertRow coords ps tr

        convertRow :: Coords -> [Player] -> [Tile]
                   -> ([Cell], [PlayerSlot], [Stuff], [Player])
        convertRow _ ps [] = ([], [], [], ps)
        convertRow coords@(Coords (x, y)) ps (t:ts) =
            ( convertTile t      : el1_4 recurse
            , fst playerLoaded  ++ el2_4 recurse
            , addStuff t coords ++ el3_4 recurse
            , el4_4 recurse
            )
            where
                recurse = convertRow (Coords (x + 1, y)) (snd playerLoaded) ts
                playerLoaded = loadPlayer t coords ps

        convertTile :: Tile -> Cell
        convertTile EmptyTile           = EmptyCell
        convertTile IndestructibleTile  = IndestructibleBlock
        convertTile PlayerStartPosition = EmptyCell
        convertTile DestructibleTile    = EmptyCell

        loadPlayer :: Tile -> Coords -> [Player] -> ([PlayerSlot], [Player])
        loadPlayer PlayerStartPosition coords (p:ps) =
            ([ConnectedPlayer NoAction p coords], ps)
        loadPlayer _ _ ps = ([], ps)

        addStuff :: Tile -> Coords -> [Stuff]
        addStuff DestructibleTile coords = [DestructibleBlock coords]
        addStuff _ _ = []

-- Actions & transitions

queueAction :: Player -> Action -> State -> State
queueAction player action (State board playerSlots stuffs bombs)
    = State board (replacePlayerAction playerSlots) stuffs bombs
    where
        replacePlayerAction :: [PlayerSlot] -> [PlayerSlot]
        replacePlayerAction [] = []
        replacePlayerAction (DisconnectedPlayer : ss) = DisconnectedPlayer : replacePlayerAction ss
        replacePlayerAction (ConnectedPlayer a p c : ss)
            | p == player = ConnectedPlayer action p c : replacePlayerAction ss
            | otherwise   = ConnectedPlayer a p c : replacePlayerAction ss

tick :: State -> State
tick s = s
