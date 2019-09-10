module Test.Sudoku
    ( mainSuite
    ) where

import Prelude

import Data.Map                     (toUnfoldable)
import Data.List                    (fromFoldable, length)
import Data.Tuple                   (Tuple(Tuple))

import Test.Unit                    (TestSuite, suite, test)
import Test.Unit.Assert as Assert

import Sudoku                       (SudokuBoard, emptyBoard, 
                                        isBoardFilled, isBoardSolved, 
                                        loadBoard, sudokuDimension)


mainSuite :: TestSuite
mainSuite = suite "Sudoku Tests" do
    test "emptyBoard tests" do
        Assert.equal' "The empty board should be larger" 
                        boardSize (length $ toUnfoldable emptyBoard)
        Assert.assertFalse "The empty board should not be filled"
                            $ isBoardFilled emptyBoard
        Assert.assertFalse "The empty board should not be solved"
                            $ isBoardSolved emptyBoard
    test "solved board tests" do
        Assert.equal' "The solved board should be larger"
                        boardSize (length $ toUnfoldable testSolvedBoard) 
        Assert.assert "The solved board should be filled"
                        $ isBoardFilled testSolvedBoard
        Assert.assert "The solved board should be solved"
                        $ isBoardSolved testSolvedBoard
    test "incorrectly board tests" do
        Assert.equal' "The incorrect board should be larger"
                        boardSize (length $ toUnfoldable testIncorrectBoard)
        Assert.assert "The incorrect board should be filled"
                        $ isBoardFilled testIncorrectBoard
        Assert.assertFalse "The incorrect board should not be solved"
                        $ isBoardSolved testIncorrectBoard

testIncorrectBoard :: SudokuBoard
testIncorrectBoard = loadBoard "445327698839654127672918543496185372218473956753296481367542819984761235521839764"

testSolvedBoard :: SudokuBoard
testSolvedBoard = loadBoard "145327698839654127672918543496185372218473956753296481367542819984761235521839764"

boardSize :: Int
boardSize = sudokuDimension * sudokuDimension

