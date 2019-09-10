module Test.Main 
    ( main
    ) where


import Prelude

import Effect                       (Effect)

import Test.Unit                    (suite, test)
import Test.Unit.Main               (runTest)
import Test.Unit.Assert as Assert

import Test.Sudoku                  (mainSuite)


main :: Effect Unit
main = runTest do
    suite "Main Tests" do
        test "placeholder" do
            Assert.equal (1 + 1) 2
    mainSuite

