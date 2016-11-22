module Main where

import Data.List
import Data.Maybe

exampleDebugMap :: [String]
exampleDebugMap =
    [ "#####"
    , "# . #"
    , "#. .#"
    , "# . #"
    , "#####"
    ]

main :: IO ()
main = do
    putStrLn ""
    putStrLn . maybe "Invalid Map" (show . opaqueState . flip startGame [p1, p2]) . mapFromDebug $ exampleDebugMap
    putStrLn ""

p1 :: Player
p1 = Player "1 foo" (Score 0) (BombCount 1) (FlameCount 1)
p2 :: Player
p2 = Player "2 bar" (Score 0) (BombCount 1) (FlameCount 1)


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
    [Maybe Player]
    [[StateSquare]]
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
                where
                    toOpaquePlayers :: [Player] -> [OpaqueItem]
                    toOpaquePlayers = fmap (\p -> OpaquePlayer . fromJust . elemIndex (Just p) $ allPlayers)
                    opaqueStuff :: Maybe Bomb -> Maybe Flame -> Maybe Powerup -> [OpaqueItem]
                    opaqueStuff b f p = catMaybes [const OpaqueBomb <$> b, const OpaqueFlame <$> f, opaquePowerup <$> p]
                        where
                            opaquePowerup :: Powerup -> OpaqueItem
                            opaquePowerup FlamePowerup = OpaqueFlamePowerup
                            opaquePowerup BombPowerup = OpaqueBombPowerup

-- Debugging

instance Show OpaqueSquare where
    show (OpaqueSquare lstItems) = toStr . foo $ lstItems
        where
            toStr :: Char -> String
            toStr c = [c]
            foo :: [OpaqueItem] -> Char
            foo [] = ' '
            foo lst = toChar . minimum $ lst
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
-- mapFromDebug = fmap Map . sequence . (fmap sequence) . ((fmap . fmap) charToTile)
mapFromDebug = fmap Map . sequence . fmap (sequence . fmap charToTile)

mapFromFile :: String -> Maybe Map
mapFromFile filename = undefined

-- Game initialization

startGame :: Map -> [Player] -> State
startGame m ps = State (Just <$> ps) (m2s m)
    where
        m2s :: Map -> [[StateSquare]]
        m2s (Map tiles2d) = (fmap . fmap) t2s tiles2d
        t2s :: Tile -> StateSquare
        t2s EmptyTile = EmptySquare
        t2s IndestructibleTile = IndestructibleBlock
        t2s DestructibleTile = DestructibleBlock
        t2s PlayerStartPosition = InterestingSquare [p1] Nothing Nothing Nothing
        -- TODO: cheating; need to incrementally assign players into positions

-- Actions & transitions

playerAction :: Player -> Action -> State -> State
playerAction = undefined

data Action = MoveUp | MoveDown | MoveLeft | MoveRight | DropBomb

bombAction :: Bomb -> State -> State
bombAction = undefined

flameAction :: Flame -> State -> State
flameAction = undefined
