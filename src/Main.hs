module Main where

import Data.List
import Data.Maybe

main :: IO ()
main = do
    putStrLn ""
    print $ opaqueState exampleState
    putStrLn ""

p1 = Player "1 foo" (Score 0) (BombCount 1) (FlameCount 1)
p2 = Player "2 bar" (Score 0) (BombCount 1) (FlameCount 1)

newtype State = State [[Square]] deriving (Show, Eq)

data Square
    = Empty
    | IndestructibleBlock
    | DestructibleBlock
    | InterestingSquare
        { players :: [Player]
        , bomb    :: Maybe Bomb
        , flame   :: Maybe Flame
        , powerup :: Maybe Powerup
        }
    deriving (Show, Eq)

data Player = Player
    { name       :: String
    , score      :: Score
    , bombCount  :: BombCount
    , flameCount :: FlameCount
    }
    deriving (Show, Eq)

data Bomb = Bomb
    { bombTicksLeft :: BombTicksLeft
    , bombOwner     :: Player
    }
    deriving (Show, Eq)

data Flame = Flame
    { flameOwner     :: Player
    }
    deriving (Show, Eq)

data Powerup = BombPowerup | FlamePowerup deriving (Show, Eq)

newtype Score = Score Integer deriving (Show, Eq)
newtype BombCount = BombCount Integer deriving (Show, Eq)
newtype FlameCount = FlameCount Integer deriving (Show, Eq)
newtype BombTicksLeft = BombTicksLeft Integer deriving (Show, Eq)


newtype OpaqueState = OpaqueState [[OpaqueSquare]] deriving (Eq)
newtype OpaqueSquare = OpaqueSquare [OpaqueItem] deriving (Eq)

data OpaqueItem
    = OpaqueFlame
    | OpaqueBomb
    | OpaquePlayer String
    | OpaqueBombPowerup
    | OpaqueFlamePowerup
    | OpaqueDestructibleBlock
    | OpaqueIndestructibleBlock
    deriving (Show, Eq, Ord)


opaqueState :: State -> OpaqueState
opaqueState (State sqList2d) = OpaqueState . (fmap . fmap) opaqueify $ sqList2d
    where
        opaqueify :: Square -> OpaqueSquare
        opaqueify Empty = OpaqueSquare []
        opaqueify IndestructibleBlock = OpaqueSquare [OpaqueIndestructibleBlock]
        opaqueify DestructibleBlock = OpaqueSquare [OpaqueDestructibleBlock]
        opaqueify (InterestingSquare players bomb flame powerup) =
            OpaqueSquare $ toOpaquePlayers players ++ opaqueStuff bomb flame powerup
                where
                    toOpaquePlayers :: [Player] -> [OpaqueItem]
                    toOpaquePlayers = fmap (\(Player name _ _ _) -> OpaquePlayer name)
                    opaqueStuff :: Maybe Bomb -> Maybe Flame -> Maybe Powerup -> [OpaqueItem]
                    opaqueStuff b f p = catMaybes [const OpaqueBomb <$> b, const OpaqueFlame <$> f, opaquePowerup <$> p]
                        where
                            opaquePowerup :: Powerup -> OpaqueItem
                            opaquePowerup FlamePowerup = OpaqueFlamePowerup
                            opaquePowerup BombPowerup = OpaqueBombPowerup

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
            toChar (OpaquePlayer (n:ns)) = n
            toChar OpaqueBombPowerup = 'b'
            toChar OpaqueFlamePowerup = 'f'

instance Show OpaqueState where
    show (OpaqueState lst2d) = join . fmap concat . opaqueify $ lst2d
        where
            join = intercalate "\n"
            opaqueify = (fmap . fmap) show


exampleState :: State
exampleState = State [[IndestructibleBlock, IndestructibleBlock, IndestructibleBlock, IndestructibleBlock, IndestructibleBlock]
                     ,[IndestructibleBlock, Empty,               DestructibleBlock,   Empty,               IndestructibleBlock]
                     ,[IndestructibleBlock, DestructibleBlock,   Empty,               DestructibleBlock,   IndestructibleBlock]
                     ,[IndestructibleBlock, Empty,               DestructibleBlock,   Empty,               IndestructibleBlock]
                     ,[IndestructibleBlock, IndestructibleBlock, IndestructibleBlock, IndestructibleBlock, IndestructibleBlock]
                     ]

-- Transitions
-- empty -> player
-- empty -> flame
-- flame -> empty
-- player..., powerup, flame, bomb -> flame
--
