module Bombastic
    ( mapFromDebug
    , DebugMap
    , mkDebugParticipant
    , Participant
    , startGame
    , State
    , tick
    , explosionResult
    , Cell (EmptyCell, Powerup)
    , PowerupVariety (..)

    , queueAction
    , Action (..)
    , Direction (..)

    , opaqueify
    , OpaqueState (..)
    , OpaqueBoard (..)
    , OpaqueCell (..)
    , OpaquePlayer (..)
    , Coords (..)
    , ParticipantName (..)
    ) where

import Data.List
import Data.Char
import Data.Tuple
import qualified Data.Sequence as S
import Data.Sequence (Seq, ViewL((:<)), (<|))
import Data.Foldable (toList)
import Data.Monoid
import System.Random


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
    StdGen
    (StdGen -> (StdGen, Cell))
    Board
    [Player]
    [BombCell]

instance Eq State where
    (==) (State _ _ b ps _) (State _ _ b' ps' _) = b == b' && ps == ps'

instance Show State where
    show (State _ _ b ps _) = show b ++ " " ++ show ps

newtype BombCell = BombCell Coords deriving (Eq, Show)

newtype Board = Board (Seq (Seq Cell)) deriving (Eq, Show)

getCell :: Board -> Coords -> Maybe Cell
getCell (Board cells) (Coords x y) =  S.lookup y cells >>= S.lookup x

replaceCell :: Board -> Coords -> Cell -> Board
replaceCell (Board cells) (Coords x y) new = Board . S.adjust' (S.update x new) y $ cells

coordsFor :: Direction -> Coords -> Coords
coordsFor Bombastic.Up    coords = coords <> Coords   0 (-1)
coordsFor Bombastic.Down  coords = coords <> Coords   0   1
coordsFor Bombastic.Left  coords = coords <> Coords (-1)  0
coordsFor Bombastic.Right coords = coords <> Coords   1   0

data Player
    = DisconnectedPlayer
    | ConnectedPlayer
        Participant
        BombCount
        FlameCount
        (Maybe Coords)
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
    = EmptyCell
    | IndestructibleBlock
    | DestructibleBlock
    | Powerup PowerupVariety
    | Bomb Participant BombTicksLeft FlameCount
    | Flame
    | FlamePendingPowerup
    deriving (Eq, Show)

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
    = OpaqueEmptyCell
    | OpaqueIndestructibleBlock
    | OpaqueDestructibleBlock
    | OpaquePowerup PowerupVariety
    | OpaqueBomb
    | OpaqueFlame
    deriving (Eq, Show)

data OpaquePlayer
    = OpaqueDisconnectedPlayer
    | OpaqueConnectedPlayer
        ParticipantName
        (Maybe Coords)
    deriving (Eq, Show)

opaqueify :: State -> OpaqueState
opaqueify (State _ _ board players _) = OpaqueState (opaqueifyBoard board) (opaqueifyPlayers players)
    where
        opaqueifyBoard (Board cells) = OpaqueBoard ((fmap . fmap) convert $ cells)
            where
                convert EmptyCell           = OpaqueEmptyCell
                convert IndestructibleBlock = OpaqueIndestructibleBlock
                convert DestructibleBlock   = OpaqueDestructibleBlock
                convert (Powerup v)         = OpaquePowerup v
                convert (Bomb _ _ _)        = OpaqueBomb
                convert Flame               = OpaqueFlame
                convert FlamePendingPowerup = OpaqueFlame

        opaqueifyPlayers ps = convert <$> ps
            where
                convert DisconnectedPlayer = OpaqueDisconnectedPlayer
                convert (ConnectedPlayer (Participant _ n) _ _ c _) = OpaqueConnectedPlayer n c

instance Show OpaqueState where
    show (OpaqueState (OpaqueBoard cells) players) = intercalate "\n" . toList $ toList <$> convertRows cells (Coords 0 0)
        where
            convertRows :: Seq (Seq OpaqueCell) -> Coords -> Seq (Seq Char)
            convertRows s coords = case S.viewl s of
                S.EmptyL -> S.empty
                r :< rs  -> convertRow r coords <| convertRows rs (incrementColCoords coords)

            convertRow :: Seq OpaqueCell -> Coords -> Seq Char
            convertRow s coords = case S.viewl s of
                S.EmptyL -> S.empty
                c :< cs  -> (addPlayer players 1 coords . convert $ c) <| convertRow cs (incrementRowCoords coords)

            incrementRowCoords c = c <> (Coords 1 0)
            incrementColCoords c = c <> (Coords 0 1)

            convert :: OpaqueCell -> Char
            convert OpaqueEmptyCell = ' '
            convert OpaqueFlame = '~'
            convert OpaqueIndestructibleBlock = '#'
            convert OpaqueDestructibleBlock = '.'
            convert (OpaquePowerup FlamePowerup) = 'f'
            convert (OpaquePowerup BombPowerup) = 'b'
            convert OpaqueBomb = 'Q'

            addPlayer :: [OpaquePlayer] -> Int -> Coords -> Char -> Char
            addPlayer _ _ _ '~'  = '~'
            addPlayer _ _ _ 'Q'  = 'Q'
            addPlayer [] _ _ repr = repr
            addPlayer (OpaqueDisconnectedPlayer:ps) n coords repr =
                addPlayer ps (n+1) coords repr
            addPlayer (OpaqueConnectedPlayer _ Nothing:ps) n coords repr =
                addPlayer ps (n+1) coords repr
            addPlayer (OpaqueConnectedPlayer _ (Just coords'):ps) n coords repr
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

startGame :: [Participant] -> StdGen -> (StdGen -> (StdGen, Cell)) -> Map -> State
startGame participants g erF (Map rows) = State g erF (Board (fst convertedRows)) (snd convertedRows) []
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
        convertRow [] (PlayerStartPosition:ts) c = (EmptyCell <| fst3 recurse, snd3 recurse, thd3 recurse)
            where
                recurse = convertRow [] ts (incrementRowCoords c)
        convertRow (pt:pts) (PlayerStartPosition:ts) c =
                ( convert PlayerStartPosition <| fst3 recurse
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

        convert EmptyTile = EmptyCell
        convert IndestructibleTile = IndestructibleBlock
        convert DestructibleTile = DestructibleBlock
        convert PlayerStartPosition = EmptyCell

        mkPlayer pt c = ConnectedPlayer pt (BombCount 1) (FlameCount 1) (Just c) NoAction

        incrementRowCoords c = c <> (Coords 1 0)
        incrementColCoords c = c <> (Coords 0 1)

queueAction :: Participant -> Action -> State -> State
queueAction participant action (State g erF board players bombCells)
    = State g erF board (replacePlayerAction players) bombCells
    where
        replacePlayerAction :: [Player] -> [Player]
        replacePlayerAction [] = []
        replacePlayerAction (DisconnectedPlayer : ps) = DisconnectedPlayer : replacePlayerAction ps
        replacePlayerAction (ConnectedPlayer ptc bc fc c a : ps)
            | ptc == participant = ConnectedPlayer ptc bc fc c (combineActions a action) : replacePlayerAction ps
            | otherwise          = ConnectedPlayer ptc bc fc c a : replacePlayerAction ps

        combineActions :: Action -> Action -> Action
        combineActions (Move dir)     DropBomb   = BombMove dir
        combineActions DropBomb       (Move dir) = BombMove dir
        combineActions (BombMove _)   (Move dir) = BombMove dir
        combineActions a@(BombMove _) DropBomb   = a
        combineActions _ a                       = a

tick :: State -> State
tick = processPlayerActions . processBombs . clearFlame
    where
        clearFlame (State g erF (Board cells) players bombCells) = State (fst blah) erF (Board . snd $ blah) players bombCells
            where
                blah :: (StdGen, Seq (Seq Cell))
                blah = (mapAccumL . mapAccumL) process g cells

                process :: StdGen -> Cell -> (StdGen, Cell)
                process g' Flame = (g', EmptyCell)
                process g' FlamePendingPowerup = erF g'
                process g' c = (g', c)

        processBombs (State g erF board players bombCells) = foldr go (State g erF board players []) bombCells
            where
                go :: BombCell -> State -> State
                go bc@(BombCell c) s@(State g' erF' b ps bcs) = case getCell b c of
                    Nothing -> s
                    Just (Bomb ptc (BombTicksLeft t) fc) -> case t of
                        1 -> explode s c ptc fc
                        _ -> State g' erF' (replaceCell b c (Bomb ptc (BombTicksLeft (t - 1)) fc)) ps (bc : bcs)
                    _ -> s -- TODO: log error? could indicate a memory-leak bug

                -- TODO: add test for topping-up of bomb count upon explosion
                explode :: State -> Coords -> Participant -> FlameCount -> State
                explode s c ptc fc =
                    topUpBombCount ptc .
                    explodeDir Bombastic.Right (coordsFor Bombastic.Right c) fc ptc . explodeDir Bombastic.Left (coordsFor Bombastic.Left c) fc ptc .
                    explodeDir Down (coordsFor Down c) fc ptc . explodeDir Up (coordsFor Up c) fc ptc $
                    ignite s c Flame

                topUpBombCount :: Participant -> State -> State
                topUpBombCount ptc (State g' erF' b ps bcs) = State g' erF' b (topUp ps) bcs
                    where
                        topUp :: [Player] -> [Player]
                        topUp [] = []
                        topUp (DisconnectedPlayer:ps') = DisconnectedPlayer : topUp ps'
                        topUp (p@(ConnectedPlayer ptc' (BombCount bc) fc c a):ps')
                            | ptc' == ptc = ConnectedPlayer ptc (BombCount (bc + 1)) fc c a : topUp ps'
                            | otherwise   = p : topUp ps'

                explodeDir :: Direction -> Coords -> FlameCount -> Participant -> State -> State
                explodeDir d c (FlameCount fc) ptc s@(State g' erF' b ps bcs) = case getCell b c of
                    Nothing -> s
                    Just cell -> case cell of
                        EmptyCell -> replacethenRecurse Flame
                        DestructibleBlock -> ignite s c FlamePendingPowerup
                        -- TODO: case where there's a powerup
                        _ -> s
                        where
                            replacethenRecurse newCell = 
                                explodeDir d (coordsFor d c) (FlameCount (fc-1)) ptc (State g' erF' (replaceCell b c newCell) ps bcs)

                ignite :: State -> Coords -> Cell -> State
                ignite (State g' erF' b ps bcs) c f = State g' erF' (replaceCell b c f) (kill c ps) bcs

                kill :: Coords -> [Player] -> [Player]
                kill _ [] = []
                kill c (p@(DisconnectedPlayer):ps) = p : kill c ps
                kill c (p@(ConnectedPlayer _ _ _ Nothing _):ps) = p : kill c ps
                kill c (p@(ConnectedPlayer ptc bc fc (Just c') _):ps)
                    | c == c'   = ConnectedPlayer ptc bc fc Nothing NoAction : kill c ps
                    | otherwise = p : kill c ps

        processPlayerActions (State g erF board players bombCells) = foldr go (State g erF board [] bombCells) players
            where
                go :: Player -> State -> State
                go p (State g' erF' b ps bcs) = case p of
                    DisconnectedPlayer -> State g' erF' b (p : ps) bcs
                    (ConnectedPlayer _ _ _ Nothing _) -> State g' erF' b (p : ps) bcs
                    (ConnectedPlayer ptc bc fc (Just coords) a) -> case a of
                        NoAction -> State g' erF' b (p : ps) bcs
                        Move dir -> State
                            g'
                            erF'
                            b
                            ( ConnectedPlayer
                                ptc
                                bc
                                fc
                                (Just . move b dir $ coords)
                                a
                            : ps
                            )
                            bcs
                        BombMove dir -> State
                            g'
                            erF'
                            (getBoard dropped)
                            ( ConnectedPlayer
                                ptc
                                (getBombCount dropped)
                                fc
                                (Just . move b dir $ coords)
                                (Move dir)
                            : ps
                            )
                            (getBombCells dropped)
                        DropBomb -> State
                            g'
                            erF'
                            (getBoard dropped)
                            ( ConnectedPlayer
                                ptc
                                (getBombCount dropped)
                                fc
                                (Just coords)
                                NoAction
                            : ps
                            )
                            (getBombCells dropped)
                        QuitGame -> State g' erF' b (p : ps) bcs -- TODO: add test & implement
                        where 
                            dropped = dropBomb b ptc bcs coords bc

                getBoard     (x, _, _) = x
                getBombCells (_, x, _) = x
                getBombCount (_, _, x) = x

                dropBomb b _ bcs _ bc@(BombCount 0) = (b, bcs, bc)
                dropBomb b ptc bcs c bc@(BombCount bci) = case getCell b c of
                    Nothing -> (b, bcs, bc)
                    (Just cell) -> case cell of
                        EmptyCell ->
                            ( replaceCell b c (Bomb ptc (BombTicksLeft 3) (FlameCount 3))
                            , BombCell c : bcs
                            , BombCount (bci-1)
                            )
                        _ -> (b, bcs, bc)

                move b d currentCoords = case getCell b newCoords of
                    Nothing   -> currentCoords
                    (Just cell) -> case cell of
                        EmptyCell -> newCoords
                        -- TODO: test for moving onto powerups
                        _         -> currentCoords
                    where
                        newCoords = coordsFor d currentCoords

-- TODO: add quickcheck test for distribution
explosionResult :: StdGen -> (StdGen, Cell)
explosionResult = fmap toCell . swap . randomR (0 :: Int, 10)
    where
        toCell :: Int -> Cell
        toCell r
            | r >= 8 && r < 10 = Powerup FlamePowerup
            | r >= 6 && r <  8 = Powerup BombPowerup
            | otherwise        = EmptyCell
