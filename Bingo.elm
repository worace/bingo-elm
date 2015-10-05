module Bingo where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (toUpper, repeat, trimRight)

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

entryList =
  ul [] [ entryItem "Future-Proof" 100,
          entryItem "Doing Agile" 200]

entryItem phrase points =
  li []
       [ span [ class "phrase" ] [text phrase],
         span [class "points"] [text (toString points)]
       ]

view =
  div [ id "container" ]
        [pageHeader,
         entryList,
         pageFooter]

main = view

