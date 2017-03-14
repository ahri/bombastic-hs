module Bombastic
    ( mapFromDebug
    , DebugMap
    , mkDebugParticipant
    , Participant
    , startGame
    , State
    , tick

    , queueAction
    , Action (..)
    , Direction (..)

    , opaqueify
    , OpaqueState (..)
    , OpaqueBoard (..)
    , OpaqueCell (..)
    , OpaquePlayer (..)
    , Coords (..)
    , Score (..)
    , ParticipantName (..)
    ) where

import Data.List
import Data.Char
import qualified Data.Sequence as S
import Data.Sequence (Seq, ViewL((:<)), (<|))
import Data.Foldable (toList)
import Data.Monoid


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
    deriving (Eq, Show)

-- TODO: switch to a 2D Seq. To ease transition see https://stackoverflow.com/questions/31106484/pattern-matching-data-sequence-like-lists
-- TODO: Reconsider players as separate list in State
newtype Board = Board (Seq (Seq Cell)) deriving (Eq, Show)

data Player
    = DisconnectedPlayer
    | ConnectedPlayer
        Participant
        Score
        BombCount
        FlameCount
        Coords
        Action
    deriving (Eq, Show)

data Participant = Participant ParticipantSecret ParticipantName deriving (Eq, Show)

mkDebugParticipant :: Int -> String -> Participant
mkDebugParticipant x n = Participant (ParticipantSecret x) (ParticipantName n)

newtype ParticipantSecret = ParticipantSecret Int deriving (Eq, Show)
newtype ParticipantName = ParticipantName String deriving (Eq, Show)

data Coords = Coords Int Int deriving (Eq, Show)
instance Monoid Coords where
    mempty = Coords 0 0
    mappend (Coords x y) (Coords x' y') = Coords (x + x') (y + y')
    mconcat = foldr mappend mempty

data Action
    = NoAction
    | Move Direction
    | MoveBomb Direction
    | DropBomb
    | QuitGame
    deriving (Eq, Show)

data Direction = Up | Down | Left | Right
    deriving (Eq, Show)

data Cell
    = EmptyCell (Maybe Flame)
    | IndestructibleBlock
    | DestructibleBlock (Maybe Flame)
    | Powerup PowerupVariety (Maybe Flame)
    | Bomb ParticipantSecret BombTicksLeft FlameCount
    deriving (Eq, Show)

newtype Flame = Flame ParticipantSecret deriving (Eq, Show)
newtype Score = Score Integer deriving (Eq, Show)
newtype BombCount = BombCount Integer deriving (Eq, Show)
newtype FlameCount = FlameCount Integer deriving (Eq, Show)
newtype BombTicksLeft = BombTicksLeft Integer deriving (Eq, Show)
data PowerupVariety = FlamePowerup | BombPowerup deriving (Eq, Show)


-- Transmission

data OpaqueState = OpaqueState
    OpaqueBoard
    [OpaquePlayer]
    deriving (Eq)

newtype OpaqueBoard = OpaqueBoard (Seq (Seq OpaqueCell)) deriving (Eq, Show)

data OpaqueCell
    = OpaqueEmptyCell (Maybe OpaqueFlame)
    | OpaqueIndestructibleBlock
    | OpaqueDestructibleBlock (Maybe OpaqueFlame)
    | OpaquePowerup PowerupVariety (Maybe OpaqueFlame)
    | OpaqueBomb
    deriving (Eq, Show)

data OpaqueFlame = OpaqueFlame deriving (Eq, Show)

data OpaquePlayer
    = OpaqueDisconnectedPlayer
    | OpaqueConnectedPlayer
        ParticipantName
        Score
        Coords
    deriving (Eq, Show)

opaqueify :: State -> OpaqueState
opaqueify (State board players) = OpaqueState (opaqueifyBoard board) (opaqueifyPlayers players)
    where
        opaqueifyBoard (Board cells) = OpaqueBoard ((fmap . fmap) convert $ cells)
            where
                convert (EmptyCell mf) = OpaqueEmptyCell (const OpaqueFlame <$> mf)
                convert IndestructibleBlock = OpaqueIndestructibleBlock
                convert (DestructibleBlock mf) = OpaqueDestructibleBlock (const OpaqueFlame <$> mf)
                convert (Powerup v mf) = OpaquePowerup v (const OpaqueFlame <$> mf)
                convert (Bomb _ _ _) = OpaqueBomb

        opaqueifyPlayers ps = convert <$> ps
            where
                convert DisconnectedPlayer = OpaqueDisconnectedPlayer
                convert (ConnectedPlayer (Participant _ n) s _ _ c _) = OpaqueConnectedPlayer n s c

instance Show OpaqueState where
    show (OpaqueState (OpaqueBoard cells) players) = intercalate "\n" . toList $ toList <$> convertRows cells (Coords 0 0)
        where
            convertRows s coords = case S.viewl s of
                S.EmptyL -> S.empty
                r :< rs  -> convertRow r coords <| convertRows rs (incrementColCoords coords)

            convertRow s coords = case S.viewl s of
                S.EmptyL -> S.empty
                c :< cs  -> (addPlayer players 1 coords . convert $ c) <| convertRow cs (incrementRowCoords coords)

            incrementRowCoords c = c <> (Coords 1 0)
            incrementColCoords c = c <> (Coords 0 1)

            convert :: OpaqueCell -> Char
            convert (OpaqueEmptyCell Nothing) = ' '
            convert (OpaqueEmptyCell (Just OpaqueFlame)) = '~'
            convert OpaqueIndestructibleBlock = '#'
            convert (OpaqueDestructibleBlock Nothing) = '.'
            convert (OpaqueDestructibleBlock (Just OpaqueFlame)) = '~'
            convert (OpaquePowerup FlamePowerup Nothing) = 'f'
            convert (OpaquePowerup FlamePowerup (Just OpaqueFlame)) = '~'
            convert (OpaquePowerup BombPowerup Nothing) = 'b'
            convert (OpaquePowerup BombPowerup (Just OpaqueFlame)) = '~'
            convert OpaqueBomb = 'Q'

            addPlayer _ _ _ '~'  = '~'
            addPlayer [] _ _ repr = repr
            addPlayer (OpaqueDisconnectedPlayer:ps) n coords repr =
                addPlayer ps (n+1) coords repr
            addPlayer (OpaqueConnectedPlayer _ _ coords':ps) n coords repr
                | coords' == coords = intToDigit n
                | otherwise = addPlayer ps (n+1) coords repr


-- Map loading

charToTile :: Char -> Maybe Tile
charToTile '#' = Just IndestructibleTile
charToTile '.' = Just DestructibleTile
charToTile ' ' = Just EmptyTile
charToTile 'S' = Just PlayerStartPosition
charToTile  _  = Nothing

mapFromDebug :: DebugMap -> Maybe Map
mapFromDebug = fmap Map . sequence . fmap (sequence . fmap charToTile)

-- Game

startGame :: [Participant] -> Map -> State
startGame participants (Map rows) = State (Board (fst convertedRows)) (snd convertedRows)
    where
        fst3 (x, _, _) = x
        snd3 (_, x, _) = x
        thd3 (_, _, x) = x

        convertedRows = convertRows participants rows (Coords 0 0)

        convertRows :: [Participant] -> [[Tile]] -> Coords -> (Seq (Seq Cell), [Player])
        convertRows _ [] _ = (S.empty, [])
        convertRows pts (ts:tss) c = (fst3 convertedRow <| fst recurse, snd3 convertedRow ++ snd recurse)
            where
                recurse = convertRows (thd3 convertedRow) tss (incrementColCoords c)
                convertedRow = convertRow pts ts c

        convertRow :: [Participant] -> [Tile] -> Coords -> (Seq Cell, [Player], [Participant])
        convertRow pts [] _ = (S.empty, [], pts) 
        convertRow [] (PlayerStartPosition:ts) c = (EmptyCell Nothing <| fst3 recurse, snd3 recurse, thd3 recurse)
            where
                recurse = convertRow [] ts (incrementRowCoords c)
        convertRow (pt:pts) (PlayerStartPosition:ts) c = (EmptyCell Nothing <| fst3 recurse, mkPlayer pt c : snd3 recurse, thd3 recurse)
            where
                recurse = convertRow pts ts (incrementRowCoords c)
        convertRow pts (t:ts) c = (convert t <| fst3 recurse, snd3 recurse, thd3 recurse)
            where
                recurse = convertRow pts ts (incrementRowCoords c)

        convert EmptyTile = EmptyCell Nothing
        convert IndestructibleTile = IndestructibleBlock
        convert DestructibleTile = DestructibleBlock Nothing
        convert PlayerStartPosition = undefined

        mkPlayer pt c = ConnectedPlayer pt (Score 0) (BombCount 1) (FlameCount 1) c NoAction

        incrementRowCoords c = c <> (Coords 1 0)
        incrementColCoords c = c <> (Coords 0 1)

queueAction :: Participant -> Action -> State -> State
queueAction participant action (State board players)
    = State board (replacePlayerAction players)
    where
        replacePlayerAction :: [Player] -> [Player]
        replacePlayerAction [] = []
        replacePlayerAction (DisconnectedPlayer : ps) = DisconnectedPlayer : replacePlayerAction ps
        replacePlayerAction (ConnectedPlayer ptc s bc fc c a : ps)
            | ptc == participant = ConnectedPlayer ptc s bc fc c (combineActions a action) : replacePlayerAction ps
            | otherwise          = ConnectedPlayer ptc s bc fc c a : replacePlayerAction ps

        combineActions :: Action -> Action -> Action
        combineActions (Move dir)    DropBomb   = MoveBomb dir
        combineActions DropBomb      (Move dir) = MoveBomb dir
        combineActions _ a = a

tick :: State -> State
tick = processPlayerActions . explodeBombs . tickBombs . clearFlame
    where
        clearFlame = id
        tickBombs = id
        explodeBombs = id
        processPlayerActions (State board players) = State board (act <$> players)
            where
                act DisconnectedPlayer = DisconnectedPlayer
                act (ConnectedPlayer ptc s bc fc coords a@(Move dir)) =
                    ConnectedPlayer ptc s bc fc (move dir coords) a
                act (ConnectedPlayer ptc s bc fc coords (MoveBomb dir)) =
                    ConnectedPlayer ptc s bc fc (move dir coords) (Move dir)
                act p@(ConnectedPlayer _ _ _ _ _ NoAction) = p
                act p@(ConnectedPlayer _ _ _ _ _ DropBomb) = p -- TODO: implement
                act p@(ConnectedPlayer _ _ _ _ _ QuitGame) = p -- TODO: implement

                move Bombastic.Up    coords = coords <> Coords   0 (-1)
                move Bombastic.Down  coords = coords <> Coords   0   1
                move Bombastic.Left  coords = coords <> Coords (-1)  0
                move Bombastic.Right coords = coords <> Coords   1   0
