module Main 
    ( main
    ) where


import Prelude

import Data.Array               (fromFoldable)
import Data.FunctorWithIndex    (mapWithIndex)
import Data.List                (List)
import Data.Maybe               (Maybe(Just, Nothing))
import Data.Tuple               (Tuple(Tuple))
import Effect                   (Effect)

import Spork.Html   as H
import Spork.Html               (Html, attr, always, input, onValueInput, 
                                style, text, value)
import Spork.PureApp            (makeWithSelector)

import Sudoku                   (BoardPosition, SudokuBoard, SudokuDigit, 
                                getRowLists, isBoardFilled, isBoardSolved, 
                                loadBoard, readDigit, setCell)


main :: Effect Unit
main = void $ makeWithSelector
    { update
    , render
    , init 
    } 
    "#app"


-- Model code

type Model = SudokuBoard

init :: Model
init = loadBoard "145327698839654127672918543496185372218473956753296481367542819984761235521839764"

-- Update code

data Action = Update BoardPosition SudokuDigit

update :: Model -> Action -> Model
update model (Update boardPos newDigit) = setCell boardPos newDigit model


-- View code

render :: Model -> Html Action
render model = 
    H.div []
    [ renderBoard model
    , boardCheckRender model
    ]

renderBoard :: Model -> Html Action
renderBoard model = case getRowLists model of
    (Just rows) -> H.div [] $ fromFoldable 
                (mapWithIndex ( \rowNum row -> renderRow rowNum row) rows)
    (Nothing) -> text "One or more rows were not recoverable from the model"

renderRow :: Int -> List SudokuDigit -> Html Action
renderRow row rowList = H.div [] $ fromFoldable
    (mapWithIndex 
        ( \col digit -> renderCell digit (Tuple row col) )
        rowList ) 

renderCell :: SudokuDigit -> BoardPosition -> Html Action
renderCell digit boardPos = 
    input [ value $ show digit
          , attr "size" "1"
          , attr "maxlength" "1"
          , onValueInput ( always $ \x -> Update boardPos (readDigit x))
          ]

boardCheckRender :: Model -> Html Action
boardCheckRender model 
    | not $ isBoardFilled model = 
        H.div [ style "color=black" ] [ text "Board not complete" ]
    | isBoardSolved model =
        H.div [ style "color=green" ] [ text "Solved" ]
    | otherwise = 
        H.div [ style "color=red" ] [ text "Incorrect solution" ]

