module Bingo where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (toUpper, repeat, trimRight)
import StartApp

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

type Action = NoOp | Sort | Delete Int

update action model =
  case action of
    NoOp -> model
    Sort -> {model | entries <- List.sortBy .points model.entries}
    Delete id ->
      let
        remainingEntries =
          List.filter (\e -> e.id /= id) model.entries
      in
        { model | entries <- remainingEntries}

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

entryList address entries =
  ul [] (List.map (entryItem address) entries)

entryItem address entry =
  li []
       [ span [ class "phrase" ] [text entry.phrase],
         span [class "points"] [text (toString entry.points)],
         button
           [ class "delete", onClick address (Delete entry.id)]
           [ ]
       ]

view address model =
  div [ id "container" ]
        [pageHeader,
         entryList address model.entries,
         button [ class "sort",
                  onClick address Sort
                ]
                [ text "Sort" ],
         pageFooter]

main =
  StartApp.start
    { model = initialModel,
      view = view,
      update = update
    }
