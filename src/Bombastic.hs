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
    [BombCell]
    deriving (Eq, Show)

newtype BombCell = BombCell Coords deriving (Eq, Show)

newtype Board = Board (Seq (Seq Cell)) deriving (Eq, Show)

getCell :: Board -> Coords -> Maybe Cell
getCell (Board cells) (Coords x y) =  S.lookup y cells >>= S.lookup x

coordsFor :: Direction -> Coords -> Coords
coordsFor Bombastic.Up    coords = coords <> Coords   0 (-1)
coordsFor Bombastic.Down  coords = coords <> Coords   0   1
coordsFor Bombastic.Left  coords = coords <> Coords (-1)  0
coordsFor Bombastic.Right coords = coords <> Coords   1   0

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
    | BombMove Direction
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
    | Bomb Participant BombTicksLeft FlameCount
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
opaqueify (State board players _) = OpaqueState (opaqueifyBoard board) (opaqueifyPlayers players)
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
            addPlayer _ _ _ 'Q'  = 'Q'
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
startGame participants (Map rows) = State (Board (fst convertedRows)) (snd convertedRows) []
    where
        fst3 (x, _, _) = x
        snd3 (_, x, _) = x
        thd3 (_, _, x) = x

        convertedRows = convertRows participants rows (Coords 0 0)

        convertRows :: [Participant] -> [[Tile]] -> Coords -> (Seq (Seq Cell), [Player])
        convertRows _ [] _ = (S.empty, [])
        convertRows pts (ts:tss) c =
                ( fst3 convertedRow <| fst recurse
                , snd3 convertedRow ++ snd recurse)
            where
                recurse = convertRows (thd3 convertedRow) tss (incrementColCoords c)
                convertedRow = convertRow pts ts c

        convertRow :: [Participant] -> [Tile] -> Coords -> (Seq Cell, [Player], [Participant])
        convertRow pts [] _ = (S.empty, [], pts) 
        convertRow [] (PlayerStartPosition:ts) c = (EmptyCell Nothing <| fst3 recurse, snd3 recurse, thd3 recurse)
            where
                recurse = convertRow [] ts (incrementRowCoords c)
        convertRow (pt:pts) (PlayerStartPosition:ts) c =
                ( EmptyCell Nothing <| fst3 recurse
                , mkPlayer pt c : snd3 recurse
                , thd3 recurse
                )
            where
                recurse = convertRow pts ts (incrementRowCoords c)
        convertRow pts (t:ts) c =
                ( convert t <| fst3 recurse
                , snd3 recurse
                , thd3 recurse
                )
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
queueAction participant action (State board players bombCells)
    = State board (replacePlayerAction players) bombCells
    where
        replacePlayerAction :: [Player] -> [Player]
        replacePlayerAction [] = []
        replacePlayerAction (DisconnectedPlayer : ps) = DisconnectedPlayer : replacePlayerAction ps
        replacePlayerAction (ConnectedPlayer ptc s bc fc c a : ps)
            | ptc == participant = ConnectedPlayer ptc s bc fc c (combineActions a action) : replacePlayerAction ps
            | otherwise          = ConnectedPlayer ptc s bc fc c a : replacePlayerAction ps

        combineActions :: Action -> Action -> Action
        combineActions (Move dir)   DropBomb   = BombMove dir
        combineActions DropBomb     (Move dir) = BombMove dir
        combineActions (BombMove _) (Move dir) = BombMove dir
        combineActions a@(BombMove _) DropBomb = a
        combineActions _ a = a

tick :: State -> State
tick = processPlayerActions . explodeBombs . tickBombs . clearFlame
    where
        clearFlame = id
        tickBombs = id
        explodeBombs = id
        processPlayerActions (State board players bombCells) = foldr go (State board [] bombCells) players
            where
                go :: Player -> State -> State
                go p (State b ps bcs) = case p of
                    DisconnectedPlayer -> State b (p : ps) bcs
                    (ConnectedPlayer _ _ _ _ _ NoAction) -> State b (p : ps) bcs
                    (ConnectedPlayer ptc s bc fc coords a@(Move dir)) -> 
                        State b (ConnectedPlayer ptc s bc fc (move b dir coords) a : ps) bcs
                    (ConnectedPlayer ptc s bc fc coords (BombMove dir)) ->
                        State (fst . dropBomb b ptc bcs $ coords) (ConnectedPlayer ptc s bc fc (move b dir coords) (Move dir) : ps) (snd . dropBomb b ptc bcs $ coords)
                    (ConnectedPlayer ptc s bc fc coords DropBomb) ->
                        State (fst . dropBomb b ptc bcs $ coords) (ConnectedPlayer ptc s bc fc coords NoAction : ps) (snd . dropBomb b ptc bcs $ coords)
                    (ConnectedPlayer _ _ _ _ _ QuitGame) -> State b (p : ps) bcs -- TODO: add test & implement

                dropBomb b@(Board cells) ptc bcs c@(Coords x y) = case getCell b c of
                    Nothing -> (b, bcs)
                    (Just cell) -> case cell of
                        EmptyCell _ ->
                            ( Board . S.adjust' (S.update x (Bomb ptc (BombTicksLeft 3) (FlameCount 3))) y $ cells
                            , BombCell c : bcs
                            )
                        _           -> (b, bcs)

                move b d currentCoords = case getCell b newCoords of
                    Nothing   -> currentCoords
                    (Just cell) -> case cell of
                        EmptyCell _ -> newCoords
                        -- TODO: test for moving onto powerups
                        _           -> currentCoords
                    where
                        newCoords = coordsFor d currentCoords
