module Sudoku
    ( BoardPosition
    , SudokuBoard
    , SudokuDigit
    , emptyBoard
    , getRowLists
    , isBoardFilled
    , isBoardSolved
    , loadBoard
    , readDigit
    , setCell
    , sudokuDimension
    ) where


import Prelude

import Data.Foldable            (foldr)
import Data.FoldableWithIndex   (foldrWithIndex)
import Data.List                (List, concat, filter, length, range)
import Data.Map                 (Map, fromFoldable, insert, lookup, values)
import Data.Maybe               (Maybe(Just, Nothing))
import Data.Set as S
import Data.Set                 (Set, difference, isEmpty)
import Data.String.Utils        (toCharArray)
import Data.Traversable         (sequence)
import Data.Tuple               (Tuple(Tuple))


type BoardPosition = Tuple Int Int

type SudokuBoard = Map BoardPosition SudokuDigit

sudokuDimension :: Int
sudokuDimension = 9

dimensionIndexList :: List Int
dimensionIndexList = range 0 $ sudokuDimension - 1

data SudokuDigit = 
    None | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
    
derive instance eqSudokuDigit :: Eq SudokuDigit
derive instance ordSudokuDigit :: Ord SudokuDigit

instance showSudokuDigit :: Show SudokuDigit where
    show (None) = ""
    show (One) = "1"
    show (Two) = "2"
    show (Three) = "3"
    show (Four) = "4"
    show (Five) = "5"
    show (Six) = "6"
    show (Seven) = "7"
    show (Eight) = "8"
    show (Nine) = "9"

readDigit :: String -> SudokuDigit
readDigit "1" = One
readDigit "2" = Two
readDigit "3" = Three
readDigit "4" = Four
readDigit "5" = Five
readDigit "6" = Six
readDigit "7" = Seven
readDigit "8" = Eight
readDigit "9" = Nine
readDigit _ = None

emptyBoard :: SudokuBoard
emptyBoard = fromFoldable $ map ( \pair -> Tuple pair None ) 
                (crossProduct dimensionIndexList dimensionIndexList)

crossProduct :: forall a . List a -> List a -> List (Tuple a a)
crossProduct l1 l2 = concat 
    (map ( \col -> map ( \row -> Tuple row col ) l2) l1)

indexToPosition :: Int -> BoardPosition
indexToPosition i = let row = i / sudokuDimension
                        col = i `mod` sudokuDimension
                    in
                        Tuple row col

loadBoard :: String -> SudokuBoard
loadBoard str = 
    foldrWithIndex 
        ( \i char board -> insert (indexToPosition i) (readDigit char) board )
        emptyBoard
        (toCharArray str)

getRowAsList :: Int -> SudokuBoard -> Maybe (List SudokuDigit)
getRowAsList rowI board = 
    sequence $ map ( \n -> lookup (Tuple rowI n) board ) dimensionIndexList

getRowLists :: SudokuBoard -> Maybe (List (List SudokuDigit))
getRowLists board = 
    sequence $ map ( \n -> getRowAsList n board ) dimensionIndexList

isBoardFilled :: SudokuBoard -> Boolean
isBoardFilled board = 
    (length $ filter isNotNone (values board)) 
        == (sudokuDimension * sudokuDimension)

isNotNone :: SudokuDigit -> Boolean
isNotNone (None) = false
isNotNone _ = true

isBoardSolved :: SudokuBoard -> Boolean
isBoardSolved board =
    let rowsSolved = areCellsSolved getRowIndices board
        colsSolved = areCellsSolved getColIndices board
        cagesSolved = areCellsSolved getCageIndices board
    in
        rowsSolved && colsSolved && cagesSolved

areCellsSolved :: (Int -> List BoardPosition) -> SudokuBoard -> Boolean
areCellsSolved getFunc board =
    let indexToValid = isValidCellSet 
                        <<< (\indices -> getCells indices board) 
                        <<< getFunc
        resultList = map indexToValid dimensionIndexList
    in
        foldr (&&) true resultList
    
isValidCellSet :: Maybe (Set SudokuDigit) -> Boolean
isValidCellSet (Just s) = isEmpty $ difference s validSet
isValidCellSet (Nothing) = false

validSet :: Set SudokuDigit
validSet = S.fromFoldable [One, Two, Three, Four, Five, 
                            Six, Seven, Eight, Nine]

getCells :: List BoardPosition -> SudokuBoard -> Maybe (Set SudokuDigit)
getCells idxs board =
    case sequence $ map ( \pos -> lookup pos board) idxs of
        (Just lDigits) -> Just $ S.fromFoldable lDigits
        (Nothing) -> Nothing

getRowIndices :: Int -> List BoardPosition
getRowIndices n = map ( \i -> Tuple n i ) dimensionIndexList

getColIndices :: Int -> List BoardPosition
getColIndices n = map ( \i -> Tuple i n ) dimensionIndexList

getCageIndices :: Int -> List BoardPosition
getCageIndices n =
    let rowStart = (n / 3) * 3
        colStart = (n `mod` 3) * 3
        rowEnd = rowStart + 2
        colEnd = colStart + 2
    in
        crossProduct (range rowStart rowEnd) (range colStart colEnd)

setCell :: BoardPosition -> SudokuDigit -> SudokuBoard -> SudokuBoard
setCell = insert

