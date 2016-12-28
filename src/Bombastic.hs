module Bombastic
    ( mapFromDebug
    , DebugMap
    , PlayerName (..)
    , startGame
    , State
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
import Data.Maybe

-- Storage

newtype Map = Map [[Tile]] deriving (Eq, Show)
type DebugMap = [String]

data Tile
    = EmptyTile
    | IndestructibleTile
    | DestructibleTile
    | PlayerStartPosition
    deriving (Eq, Show)


-- State

data State = State
    Board
    [Player]
    [Stuff]
    [Bomb]
    deriving (Eq, Show)

newtype Board = Board [[Cell]] deriving (Eq, Show)

data Player
    = DisconnectedPlayer
    | ConnectedPlayer
        PlayerName
        Score
        BombCount
        FlameCount
        Coords
        Action
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
    | MoveUpBomb
    | MoveDown
    | MoveDownBomb
    | MoveLeft
    | MoveLeftBomb
    | MoveRight
    | MoveRightBomb
    | DropBomb
    | QuitGame
    deriving (Eq, Show)

data Cell
    = EmptyCell
    | IndestructibleBlock
    deriving (Eq, Show)

newtype PlayerName = PlayerName String deriving (Eq, Show)

data Bomb = Bomb
    PlayerName
    Coords
    BombTicksLeft
    FlameCount
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
                        | x == x' && y == y' = Just 'Q'
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
opaqueify (State board players stuffs bombs) = OpaqueState
    board
    (opaqueifyPlayer <$> players)
    (opaqueifyStuff      <$> stuffs)
    (opaqueifyBomb       <$> bombs)
    where
        opaqueifyPlayer :: Player -> OpaquePlayer
        opaqueifyPlayer DisconnectedPlayer      = OpaqueDisconnectedPlayer
        opaqueifyPlayer (ConnectedPlayer _ _ _ _ c _) = OpaqueConnectedPlayer c

        opaqueifyStuff :: Stuff -> OpaqueStuff
        opaqueifyStuff (DestructibleBlock c) = OpaqueDestructibleBlock c
        opaqueifyStuff (Flame c _)           = OpaqueFlame c
        opaqueifyStuff (FlamePowerup c)      = OpaqueFlamePowerup c
        opaqueifyStuff (BombPowerup c)       = OpaqueBombPowerup c

        opaqueifyBomb :: Bomb -> OpaqueBomb
        opaqueifyBomb (Bomb _ c _ _) = OpaqueBomb c


-- Map loading

charToTile :: Char -> Maybe Tile
charToTile '#' = Just IndestructibleTile
charToTile '.' = Just DestructibleTile
charToTile ' ' = Just EmptyTile
charToTile 'S' = Just PlayerStartPosition
charToTile  _  = Nothing

mapFromDebug :: DebugMap -> Maybe Map
mapFromDebug = fmap Map . sequence . fmap (sequence . fmap charToTile)


-- Game initialization

startGame :: [PlayerName] -> Map -> State
startGame playerNames (Map tiles2d) = State board players stuffs []
    where
        board = Board . el1_3 $ converted
        players = el2_3 converted
        stuffs = el3_3 converted

        el1_3 (el, _, _) = el
        el2_3 (_, el, _) = el
        el3_3 (_, _, el) = el

        el1_4 (el, _, _, _) = el
        el2_4 (_, el, _, _) = el
        el3_4 (_, _, el, _) = el
        el4_4 (_, _, _, el) = el

        converted = convert (Coords (0, 0)) playerNames tiles2d

        convert :: Coords -> [PlayerName] -> [[Tile]]
                -> ([[Cell]], [Player], [Stuff])
        convert _ _ [] = ([], [], [])
        convert coords@(Coords (x, y)) ps (tr:trs) =
            ( el1_4 convertedRow  : el1_3 recurse
            , el2_4 convertedRow ++ el2_3 recurse
            , el3_4 convertedRow ++ el3_3 recurse
            )
            where
                recurse = convert (Coords (x, y + 1)) (el4_4 convertedRow) trs
                convertedRow = convertRow coords ps tr

        convertRow :: Coords -> [PlayerName] -> [Tile]
                   -> ([Cell], [Player], [Stuff], [PlayerName])
        convertRow _ pns [] = ([], [], [], pns)
        convertRow coords@(Coords (x, y)) pns (t:ts) =
            ( convertTile t      : el1_4 recurse
            , fst playerLoaded  ++ el2_4 recurse
            , addStuff t coords ++ el3_4 recurse
            , el4_4 recurse
            )
            where
                recurse = convertRow (Coords (x + 1, y)) (snd playerLoaded) ts
                playerLoaded = loadPlayer t coords pns

        convertTile :: Tile -> Cell
        convertTile EmptyTile           = EmptyCell
        convertTile IndestructibleTile  = IndestructibleBlock
        convertTile PlayerStartPosition = EmptyCell
        convertTile DestructibleTile    = EmptyCell

        loadPlayer :: Tile -> Coords -> [PlayerName] -> ([Player], [PlayerName])
        loadPlayer PlayerStartPosition coords (pn:pns) =
            ([ConnectedPlayer pn (Score 0) (BombCount 1) (FlameCount 1) coords NoAction], pns)
        loadPlayer _ _ pns = ([], pns)

        addStuff :: Tile -> Coords -> [Stuff]
        addStuff DestructibleTile coords = [DestructibleBlock coords]
        addStuff _ _ = []

-- Actions & transitions

-- TODO: players need to be given a generated token with which to execute actions, obviously this must not be in their opaque representation
-- currently the lookup is purely on player name...
queueAction :: PlayerName -> Action -> State -> State
queueAction playerName action (State board players stuffs bombs)
    = State board (replacePlayerAction players) stuffs bombs
    where
        replacePlayerAction :: [Player] -> [Player]
        replacePlayerAction [] = []
        replacePlayerAction (DisconnectedPlayer : ps) = DisconnectedPlayer : replacePlayerAction ps
        replacePlayerAction (ConnectedPlayer pn s bc fc c a : ps)
            | pn == playerName = ConnectedPlayer pn s bc fc c (combineActions a action) : replacePlayerAction ps
            | otherwise   = ConnectedPlayer pn s bc fc c a : replacePlayerAction ps

        combineActions :: Action -> Action -> Action
        combineActions MoveUp        DropBomb  = MoveLeftBomb
        combineActions DropBomb      MoveUp    = MoveLeftBomb
        combineActions MoveDown      DropBomb  = MoveDownBomb
        combineActions DropBomb      MoveDown  = MoveDownBomb
        combineActions MoveLeft      DropBomb  = MoveLeftBomb
        combineActions DropBomb      MoveLeft  = MoveLeftBomb
        combineActions MoveRight     DropBomb  = MoveRightBomb
        combineActions DropBomb      MoveRight = MoveRightBomb
        combineActions MoveUpBomb    MoveDown  = MoveDownBomb
        combineActions MoveUpBomb    MoveLeft  = MoveLeftBomb
        combineActions MoveUpBomb    MoveRight = MoveRightBomb
        combineActions MoveUpBomb    DropBomb  = MoveUpBomb
        combineActions MoveDownBomb  MoveUp    = MoveUpBomb
        combineActions MoveDownBomb  MoveLeft  = MoveLeftBomb
        combineActions MoveDownBomb  MoveRight = MoveRightBomb
        combineActions MoveDownBomb  DropBomb  = MoveDownBomb
        combineActions MoveLeftBomb  MoveUp    = MoveUpBomb
        combineActions MoveLeftBomb  MoveDown  = MoveDownBomb
        combineActions MoveLeftBomb  MoveRight = MoveRightBomb
        combineActions MoveLeftBomb  DropBomb  = MoveLeftBomb
        combineActions MoveRightBomb MoveUp    = MoveUpBomb
        combineActions MoveRightBomb MoveDown  = MoveDownBomb
        combineActions MoveRightBomb MoveLeft  = MoveLeftBomb
        combineActions MoveRightBomb DropBomb  = MoveRightBomb
        combineActions _ a = a

tick :: State -> State
tick (State board players stuffs bombs) = -- TODO: maybe this should be a recursive algorithm, now it's getting more complicated
        State
            board
            (fst . processPlayer <$> players)
            stuffs
            (bombs ++ catMaybes (snd . processPlayer <$> players))
    where
        processPlayer :: Player -> (Player, Maybe Bomb)
        processPlayer dp@DisconnectedPlayer = (dp, Nothing)

        processPlayer cp@(ConnectedPlayer _ _ _ _ _ NoAction) = (cp, Nothing)
        processPlayer (ConnectedPlayer n s bc fc c a@DropBomb) = (ConnectedPlayer n s (fst . dropBomb n c bc $ fc) fc c a, snd . dropBomb n c bc $ fc)
        processPlayer cp@(ConnectedPlayer _ _ _ _ _ QuitGame) = (cp, Nothing) -- TODO: implement

        processPlayer (ConnectedPlayer n s bc fc c a@MoveUp)        = (ConnectedPlayer n s bc                           fc (fst . moveUp a $ c)    (snd . moveUp a $ c),    Nothing)
        processPlayer (ConnectedPlayer n s bc fc c a@MoveUpBomb)    = (ConnectedPlayer n s (fst . dropBomb n c bc $ fc) fc (fst . moveUp a $ c)    (snd . moveUp a $ c),    snd . dropBomb n c bc $ fc)
        processPlayer (ConnectedPlayer n s bc fc c a@MoveDown)      = (ConnectedPlayer n s bc                           fc (fst . moveDown a $ c)  (snd . moveDown a $ c),  Nothing)
        processPlayer (ConnectedPlayer n s bc fc c a@MoveDownBomb)  = (ConnectedPlayer n s (fst . dropBomb n c bc $ fc) fc (fst . moveDown a $ c)  (snd . moveDown a $ c),  snd . dropBomb n c bc $ fc)
        processPlayer (ConnectedPlayer n s bc fc c a@MoveLeft)      = (ConnectedPlayer n s bc                           fc (fst . moveLeft a $ c)  (snd . moveLeft a $ c),  Nothing)
        processPlayer (ConnectedPlayer n s bc fc c a@MoveLeftBomb)  = (ConnectedPlayer n s (fst . dropBomb n c bc $ fc) fc (fst . moveLeft a $ c)  (snd . moveLeft a $ c),  snd . dropBomb n c bc $ fc)
        processPlayer (ConnectedPlayer n s bc fc c a@MoveRight)     = (ConnectedPlayer n s bc                           fc (fst . moveRight a $ c) (snd . moveRight a $ c), Nothing)
        processPlayer (ConnectedPlayer n s bc fc c a@MoveRightBomb) = (ConnectedPlayer n s (fst . dropBomb n c bc $ fc) fc (fst . moveRight a $ c) (snd . moveRight a $ c), snd . dropBomb n c bc $ fc)

        moveUp    a c@(Coords (x, y)) = move c a (Coords (x, y-1))
        moveDown  a c@(Coords (x, y)) = move c a (Coords (x, y+1))
        moveLeft  a c@(Coords (x, y)) = move c a (Coords (x-1, y))
        moveRight a c@(Coords (x, y)) = move c a (Coords (x+1, y))

        dropBomb :: PlayerName -> Coords -> BombCount -> FlameCount -> (BombCount, Maybe Bomb)
        dropBomb n c bc@(BombCount i) fc
            | i > 0 = (BombCount (i - 1), Just (Bomb n c (BombTicksLeft 3) fc))
            | otherwise = (bc, Nothing)

        move :: Coords -> Action -> Coords -> (Coords, Action)
        move from action to
            | indestructibleBlockAt board to = (from, NoAction)
            | destructibleBlockAt to = (from, NoAction)
            | bombAt to = (from, NoAction)
            | otherwise = (to, reduceAction action)

        reduceAction :: Action -> Action
        reduceAction MoveUpBomb    = MoveUp
        reduceAction MoveDownBomb  = MoveDown
        reduceAction MoveLeftBomb  = MoveLeft
        reduceAction MoveRightBomb = MoveRight
        reduceAction a = a

        indestructibleBlockAt :: Board -> Coords -> Bool
        indestructibleBlockAt (Board cells2d) (Coords (x, y)) =
            ((cells2d !! y) !! x) == IndestructibleBlock

        destructibleBlockAt :: Coords -> Bool
        destructibleBlockAt coords = any search stuffs
            where
                search :: Stuff -> Bool
                search (DestructibleBlock coords') = coords == coords'
                search _ = False

        bombAt :: Coords -> Bool
        bombAt coords = any search bombs
            where
                search :: Bomb -> Bool
                search (Bomb _ coords' _ _) = coords == coords'
