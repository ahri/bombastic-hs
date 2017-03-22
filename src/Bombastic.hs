module Bombastic
    ( mapFromDebug
    , DebugMap
    , mkDebugParticipant
    , Participant
    , startGame
    , GameState
    , tick
    , explosionResult
    , Cell (EmptyCell, Powerup)
    , PowerupVariety (..)

    , queueAction
    , Action (..)
    , Direction (..)

    , opaqueify
    , OpaqueGameState (..)
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
    | PowerupTile PowerupVariety
    deriving (Eq, Show)


-- State

data GameState = GameState
    StdGen
    (StdGen -> (StdGen, Cell))
    Board
    [Player]
    [BombCell]

instance Eq GameState where
    (==) (GameState _ _ b ps _) (GameState _ _ b' ps' _) = b == b' && ps == ps'

instance Show GameState where
    show (GameState _ _ b ps _) = show b ++ " " ++ show ps

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

data NoLongerPlayingReason = Quit | Killed deriving (Eq, Show)
data Player
    = NoLongerPlaying Participant NoLongerPlayingReason
    | ConnectedPlayer
        Participant
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

data OpaqueGameState = OpaqueGameState
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
    = OpaqueNoLongerPlaying ParticipantName
    | OpaqueConnectedPlayer
        ParticipantName
        Coords
    deriving (Eq, Show)

opaqueify :: GameState -> OpaqueGameState
opaqueify (GameState _ _ board players _) = OpaqueGameState (opaqueifyBoard board) (opaqueifyPlayers players)
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
                convert (NoLongerPlaying (Participant _ n) _) = OpaqueNoLongerPlaying n
                convert (ConnectedPlayer (Participant _ n) _ _ c _) = OpaqueConnectedPlayer n c

instance Show OpaqueGameState where
    show (OpaqueGameState (OpaqueBoard cells) players) = intercalate "\n" . toList $ toList <$> convertRows cells (Coords 0 0)
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
            convert OpaqueDestructibleBlock = '+'
            convert (OpaquePowerup FlamePowerup) = 'f'
            convert (OpaquePowerup BombPowerup) = 'b'
            convert OpaqueBomb = 'Q'

            addPlayer :: [OpaquePlayer] -> Int -> Coords -> Char -> Char
            addPlayer _ _ _ '~'  = '~'
            addPlayer _ _ _ 'Q'  = 'Q'
            addPlayer [] _ _ repr = repr
            addPlayer (OpaqueNoLongerPlaying _:ps) n coords repr =
                addPlayer ps (n+1) coords repr
            addPlayer (OpaqueConnectedPlayer _ coords':ps) n coords repr
                | coords' == coords = intToDigit n
                | otherwise = addPlayer ps (n+1) coords repr


-- Map loading

charToTile :: Char -> Maybe Tile
charToTile '#' = Just IndestructibleTile
charToTile '+' = Just DestructibleTile
charToTile ' ' = Just EmptyTile
charToTile 'S' = Just PlayerStartPosition
charToTile 'b' = Just . PowerupTile $ BombPowerup
charToTile 'f' = Just . PowerupTile $ FlamePowerup
charToTile  _  = Nothing

mapFromDebug :: DebugMap -> Maybe Map
mapFromDebug = fmap Map . sequence . fmap (sequence . fmap charToTile)

-- Game

startGame :: [Participant] -> StdGen -> (StdGen -> (StdGen, Cell)) -> Map -> GameState
startGame participants g erF (Map rows) = GameState g erF (Board (fst convertedRows)) (snd convertedRows) []
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
        convert (PowerupTile v) = Powerup v

        mkPlayer pt c = ConnectedPlayer pt (BombCount 1) (FlameCount 1) c NoAction

        incrementRowCoords c = c <> (Coords 1 0)
        incrementColCoords c = c <> (Coords 0 1)

queueAction :: Participant -> Action -> GameState -> GameState
queueAction participant action (GameState g erF board players bombCells)
    = GameState g erF board (replacePlayerAction players) bombCells
    where
        replacePlayerAction :: [Player] -> [Player]
        replacePlayerAction [] = []
        replacePlayerAction (p@(NoLongerPlaying _ _):ps) = p : replacePlayerAction ps
        replacePlayerAction (ConnectedPlayer ptc bc fc c a : ps)
            | ptc == participant = ConnectedPlayer ptc bc fc c (combineActions a action) : replacePlayerAction ps
            | otherwise          = ConnectedPlayer ptc bc fc c a : replacePlayerAction ps

        combineActions :: Action -> Action -> Action
        combineActions (Move dir)     DropBomb   = BombMove dir
        combineActions DropBomb       (Move dir) = BombMove dir
        combineActions (BombMove _)   (Move dir) = BombMove dir
        combineActions a@(BombMove _) DropBomb   = a
        combineActions _ a                       = a

tick :: GameState -> GameState
tick = processPlayerActions . processBombs . clearFlame
    where
        clearFlame (GameState g erF (Board cells) players bombCells) = GameState (fst blah) erF (Board . snd $ blah) players bombCells
            where
                blah :: (StdGen, Seq (Seq Cell))
                blah = (mapAccumL . mapAccumL) process g cells

                process :: StdGen -> Cell -> (StdGen, Cell)
                process g' Flame = (g', EmptyCell)
                process g' FlamePendingPowerup = erF g'
                process g' c = (g', c)

        processBombs (GameState g erF board players bombCells) = foldr go (GameState g erF board players []) bombCells
            where
                go :: BombCell -> GameState -> GameState
                go bc@(BombCell c) s@(GameState g' erF' b ps bcs) = case getCell b c of
                    Nothing -> s
                    Just (Bomb ptc (BombTicksLeft t) fc) -> case t of
                        1 -> explode s c ptc fc
                        _ -> GameState g' erF' (replaceCell b c (Bomb ptc (BombTicksLeft (t - 1)) fc)) ps (bc : bcs)
                    _ -> s -- TODO: log error? could indicate a memory-leak bug

                explode :: GameState -> Coords -> Participant -> FlameCount -> GameState
                explode s c ptc fc =
                    topUpBombCount ptc .
                    explodeDir Bombastic.Right (coordsFor Bombastic.Right c) fc ptc . explodeDir Bombastic.Left (coordsFor Bombastic.Left c) fc ptc .
                    explodeDir Down (coordsFor Down c) fc ptc . explodeDir Up (coordsFor Up c) fc ptc $
                    ignite s c Flame

                topUpBombCount :: Participant -> GameState -> GameState
                topUpBombCount ptc (GameState g' erF' b ps bcs) = GameState g' erF' b (topUp ps) bcs
                    where
                        topUp :: [Player] -> [Player]
                        topUp [] = []
                        topUp (p@(NoLongerPlaying _ _):ps') = p : topUp ps'
                        topUp (p@(ConnectedPlayer ptc' (BombCount bc) fc c a):ps')
                            | ptc' == ptc = ConnectedPlayer ptc (BombCount (bc + 1)) fc c a : topUp ps'
                            | otherwise   = p : topUp ps'

                explodeDir :: Direction -> Coords -> FlameCount -> Participant -> GameState -> GameState
                explodeDir _ _ (FlameCount 0) _ s = s
                explodeDir d c fc@(FlameCount fc') ptc s@(GameState _ _ b _ _) = case getCell b c of
                    Nothing -> s
                    Just cell -> case cell of
                        EmptyCell           -> recurse . ign $ Flame
                        Flame               -> recurse s
                        FlamePendingPowerup -> recurse s
                        DestructibleBlock   -> ign FlamePendingPowerup
                        Powerup _           -> recurse . ign $ Flame
                        Bomb _ _ _          -> explode s c ptc fc
                        _                   -> s
                        where
                            recurse :: GameState -> GameState
                            recurse = explodeDir d (coordsFor d c) (FlameCount (fc'-1)) ptc

                            ign :: Cell -> GameState
                            ign newCell = ignite s c newCell

                ignite :: GameState -> Coords -> Cell -> GameState
                ignite (GameState g' erF' b ps bcs) c f = GameState g' erF' (replaceCell b c f) (kill c ps) bcs

                kill :: Coords -> [Player] -> [Player]
                kill _ [] = []
                kill c (p@(NoLongerPlaying _ _):ps) = p : kill c ps
                kill c (p@(ConnectedPlayer ptc _ _ c' _):ps)
                    | c == c'   = NoLongerPlaying ptc Killed : kill c ps
                    | otherwise = p : kill c ps

        processPlayerActions (GameState g erF board players bombCells) = foldr go (GameState g erF board [] bombCells) players
            where
                go :: Player -> GameState -> GameState
                go p (GameState g' erF' b ps bcs) = case p of
                    NoLongerPlaying _ _ -> GameState g' erF' b (p : ps) bcs
                    (ConnectedPlayer ptc bc fc coords a) -> case a of
                        NoAction -> GameState g' erF' b (p : ps) bcs
                        Move dir -> let
                                (movedBoard, movedBc, movedFc, movedCoords) = move b bc fc dir coords
                            in GameState
                                g'
                                erF'
                                movedBoard
                                ( ConnectedPlayer
                                    ptc
                                    movedBc
                                    movedFc
                                    movedCoords
                                    a
                                : ps
                                )
                                bcs
                        BombMove dir -> let
                                (droppedBoard, droppedBombCells, droppedBombCount) = dropBomb b ptc bcs coords bc fc
                                (movedBoard, movedBc, movedFc, movedCoords) = move droppedBoard droppedBombCount fc dir coords
                            in GameState
                                g'
                                erF'
                                movedBoard
                                ( ConnectedPlayer
                                    ptc
                                    movedBc
                                    movedFc
                                    movedCoords
                                    (Move dir)
                                : ps
                                )
                                droppedBombCells
                        DropBomb -> let
                                (droppedBoard, droppedBombCells, droppedBombCount) = dropBomb b ptc bcs coords bc fc
                            in GameState
                                g'
                                erF'
                                droppedBoard
                                ( ConnectedPlayer
                                    ptc
                                    droppedBombCount
                                    fc
                                    coords
                                    NoAction
                                : ps
                                )
                                droppedBombCells
                        QuitGame -> GameState g' erF' b (p : ps) bcs -- TODO: add test & implement

                dropBomb b _ bcs _ bc@(BombCount 0) _ = (b, bcs, bc)
                dropBomb b ptc bcs c bc@(BombCount bci) fc = case getCell b c of
                    Nothing -> (b, bcs, bc)
                    (Just cell) -> case cell of
                        EmptyCell ->
                            ( replaceCell b c (Bomb ptc (BombTicksLeft 3) fc)
                            , BombCell c : bcs
                            , BombCount (bci-1)
                            )
                        _ -> (b, bcs, bc)

                move :: Board -> BombCount -> FlameCount -> Direction -> Coords -> (Board, BombCount, FlameCount, Coords)
                move b bc@(BombCount bc') fc@(FlameCount fc') d currentCoords = case getCell b newCoords of
                    Nothing   -> (b, bc, fc, currentCoords)
                    (Just cell) -> case cell of
                        EmptyCell -> (b, bc, fc, newCoords)
                        Powerup FlamePowerup -> (replaceCell b newCoords EmptyCell, bc, FlameCount (fc'+1), newCoords)
                        Powerup BombPowerup  -> (replaceCell b newCoords EmptyCell, BombCount (bc'+1), fc,  newCoords)
                        _         -> (b, bc, fc, currentCoords)
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
