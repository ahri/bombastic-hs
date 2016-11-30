module Bombastic (getPlayers, mkPlayer, opaqueState, startGame, mapFromDebug) where

import Data.List
import Data.Maybe

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
    [PlayerSlot]
    [[StateSquare]]
    deriving (Eq, Show)

getPlayers :: State -> [PlayerSlot]
getPlayers (State ps _) = ps

data PlayerSlot
    = DisconnectedPlayer
    | ConnectedPlayer
        Action
        Player
    deriving (Eq, Show)

data Action
    = NoAction
    | MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight
    | DropBomb
    | QuitGame
    deriving (Eq, Show)

data StateSquare
    = EmptySquare
    | IndestructibleBlock
    | DestructibleBlock
    | InterestingSquare
        [Player]
        (Maybe Bomb)
        (Maybe Flame)
        (Maybe Powerup)
    deriving (Eq, Show)

data Player = Player
    String
    Score
    BombCount
    FlameCount
    deriving (Eq, Show)

mkPlayer :: String -> Player
mkPlayer name = Player name (Score 0) (BombCount 1) (FlameCount 1)

data Bomb = Bomb
    BombTicksLeft
    Player
    deriving (Eq, Show)

data Flame = Flame
    Player
    deriving (Eq, Show)

data Powerup = BombPowerup | FlamePowerup deriving (Eq, Show)

newtype Score = Score Integer deriving (Eq, Show)
newtype BombCount = BombCount Integer deriving (Eq, Show)
newtype FlameCount = FlameCount Integer deriving (Eq, Show)
newtype BombTicksLeft = BombTicksLeft Integer deriving (Eq, Show)


-- Transmission

newtype OpaqueState = OpaqueState [[OpaqueSquare]] deriving (Eq)
newtype OpaqueSquare = OpaqueSquare [OpaqueItem] deriving (Eq)

data OpaqueItem
    = OpaqueFlame
    | OpaqueBomb
    | OpaquePlayer Int
    | OpaqueBombPowerup
    | OpaqueFlamePowerup
    | OpaqueDestructibleBlock
    | OpaqueIndestructibleBlock
    deriving (Eq, Ord, Show)


-- TODO: should be State -> Maybe OpaqueState; if players exist on a square that are not in the overall game state, I shouldn't be able to serialize it. I'm currently cheating using fromJust - so maybe use sequence?
opaqueState :: State -> OpaqueState
opaqueState (State allPlayers sqList2d) = OpaqueState . (fmap . fmap) opaqueify $ sqList2d
    where
        opaqueify :: StateSquare -> OpaqueSquare
        opaqueify EmptySquare = OpaqueSquare []
        opaqueify IndestructibleBlock = OpaqueSquare [OpaqueIndestructibleBlock]
        opaqueify DestructibleBlock = OpaqueSquare [OpaqueDestructibleBlock]
        opaqueify (InterestingSquare players bomb flame powerup) =
            OpaqueSquare $ toOpaquePlayers players ++ opaqueStuff bomb flame powerup

        toOpaquePlayers :: [Player] -> [OpaqueItem]
        toOpaquePlayers = fmap (\p -> OpaquePlayer . fromJust . elemIndex p $ rawPlayers)
            where
                rawPlayers = mapMaybe search allPlayers
                search DisconnectedPlayer = Nothing
                search (ConnectedPlayer _ p) = Just p

        opaqueStuff :: Maybe Bomb -> Maybe Flame -> Maybe Powerup -> [OpaqueItem]
        opaqueStuff b f p = catMaybes [OpaqueBomb <$ b, OpaqueFlame <$ f, opaquePowerup <$> p]

        opaquePowerup :: Powerup -> OpaqueItem
        opaquePowerup FlamePowerup = OpaqueFlamePowerup
        opaquePowerup BombPowerup = OpaqueBombPowerup


-- Debugging

instance Show OpaqueSquare where
    show (OpaqueSquare lstItems) = toStr . osToChar $ lstItems
        where
            toStr :: Char -> String
            toStr c = [c]

            osToChar :: [OpaqueItem] -> Char
            osToChar [] = ' '
            osToChar lst = toChar . minimum $ lst

            toChar :: OpaqueItem -> Char
            toChar OpaqueIndestructibleBlock = '#'
            toChar OpaqueDestructibleBlock = '.'
            toChar OpaqueFlame = '~'
            toChar OpaqueBomb = 'o'
            toChar (OpaquePlayer p) = head . show $ p
            toChar OpaqueBombPowerup = 'b'
            toChar OpaqueFlamePowerup = 'f'

instance Show OpaqueState where
    show (OpaqueState lst2d) = join . fmap concat . opaqueify $ lst2d
        where
            join = intercalate "\n"
            opaqueify = (fmap . fmap) show


-- Map loading

charToTile :: Char -> Maybe Tile
charToTile '#' = Just IndestructibleTile
charToTile '.' = Just DestructibleTile
charToTile ' ' = Just EmptyTile
charToTile 'S' = Just PlayerStartPosition
charToTile  _  = Nothing

mapFromDebug :: [String] -> Maybe Map
mapFromDebug = fmap Map . sequence . fmap (sequence . fmap charToTile)

-- mapFromFile :: String -> Maybe Map
-- mapFromFile filename = undefined


-- Game initialization

startGame :: [Player] -> Map -> State
startGame ps (Map tiles2d) = State playerSlots stateSquares
    where
        stateSquares = fst3 result
        playerSlots :: [PlayerSlot]
        playerSlots = (ConnectedPlayer NoAction) <$> playersIncluded
        playersIncluded = filter (`notElem` playersLeftOver) ps
        playersLeftOver = snd3 result
        result = m2s [] ps tiles2d

        fst3 (e, _, _) = e
        snd3 (_, e, _) = e

        m2s :: [[StateSquare]] -> [Player] -> [[Tile]] -> ([[StateSquare]], [Player], [[Tile]])
        m2s ssA psR [] = (ssA, psR, [])
        m2s ssA psR (r : rs) = m2s stateSquaresAccumulator playersReducer rs
            where
                stateSquaresAccumulator = ssA ++ [fst3 (r2s [] psR r)]
                playersReducer = snd3 (r2s [] psR r)

        r2s :: [StateSquare] -> [Player] -> [Tile] -> ([StateSquare], [Player], [Tile])
        r2s ssA psR [] = (ssA, psR, [])
        r2s ssA [] (PlayerStartPosition : tsR) = r2s (ssA ++ [EmptySquare]) [] tsR
        r2s ssA (p : psR) (PlayerStartPosition : tsR) = r2s (ssA ++ [InterestingSquare [p] Nothing Nothing Nothing]) psR tsR
        r2s ssA psR (t : tsR) = r2s (ssA ++ [t2s t]) psR tsR

        t2s :: Tile -> StateSquare
        t2s EmptyTile = EmptySquare
        t2s PlayerStartPosition = EmptySquare
        t2s IndestructibleTile = IndestructibleBlock
        t2s DestructibleTile = DestructibleBlock


-- Actions & transitions

-- playerAction :: Player -> Action -> State -> State
-- playerAction = undefined
-- 
-- bombAction :: Bomb -> State -> State
-- bombAction = undefined
-- 
-- flameAction :: Flame -> State -> State
-- flameAction = undefined
