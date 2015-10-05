module Bingo where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (toUpper, repeat, trimRight)

-- MODEL

newEntry phrase points id =
  { phrase = phrase,
    points = points,
    wasSpoken = False,
    id = id
  }

initialModel =
  {
    entries = [ newEntry "Future-Proof" 100 1,
               newEntry "In the Cloud" 300 3,
               newEntry "Doing Agile" 200 2]
  }

-- Update

type Action = NoOp | Sort

update action model =
  case action of
    NoOp -> model
    Sort -> {model | entries <- List.sortBy .points model.entries}

-- View

title message times =
  message ++ " "
    |> toUpper
    |> repeat times
    |> trimRight
    |> text


pageHeader =
  h1 [ id "logo", class "classy" ] [ title "bingo!" 3 ]

pageFooter =
  footer [ ]
         [ a [ href "https://pragmaticstudio.com",
               target "blank"
             ]
             [text "Prag Studio"]
         ]

entryList entries =
  ul [] (List.map entryItem entries)

entryItem entry =
  li []
       [ span [ class "phrase" ] [text entry.phrase],
         span [class "points"] [text (toString entry.points)]
       ]

view model =
  div [ id "container" ]
        [pageHeader,
         entryList model.entries,
         pageFooter]

-- Wire together

main =
  initialModel
  |> update Sort
  |> view

