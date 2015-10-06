module Bingo where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (toUpper, repeat, trimRight)
import Signal exposing (Address)
import StartApp
import BingoUtils as Utils

-- MODEL

type alias Entry =
  {phrase: String, points: Int, wasSpoken: Bool, id: Int}

type alias Model =
                 {
                   entries: List Entry,
                   phraseInput: String,
                   pointsInput: String,
                   nextID: Int
                 }

initialModel : Model
initialModel =
  {
    entries = [ Entry "Future-Proof" 100 False 1,
                Entry "In the Cloud" 300 False 3,
                Entry "Rock-Star Ninja" 400 False 4,
                Entry "Doing Agile" 200 False 2],
    phraseInput = "",
    pointsInput = "",
    nextID = 5
  }

-- Update

type Action
  = NoOp
  | Sort
  | Delete Int
  | Mark Int
  | UpdatePhraseInput String
  | UpdatePointsInput String
  | AddEntry

update : Action -> Model -> Model
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
    Mark id ->
      let
        updateEntry e =
          if e.id == id
          then { e | wasSpoken <- (not e.wasSpoken) }
          else e
      in
        { model | entries <- List.map updateEntry model.entries}
    UpdatePhraseInput contents ->
      { model | phraseInput <- contents }
    UpdatePointsInput contents ->
      { model | pointsInput <- contents}
    AddEntry ->
      let
        entryToAdd =
          Entry model.phraseInput (Utils.parseInt model.pointsInput) False model.nextID
        isInvalid model =
          String.isEmpty model.phraseInput || String.isEmpty model.pointsInput
      in
        if isInvalid model
        then model
        else
          {model |
           phraseInput <- "",
           pointsInput <- "",
           entries <- entryToAdd :: model.entries,
           nextID <- model.nextID + 1
          }

totalPoints : List Entry -> Int
totalPoints entries =
  let
    spokenEntries = List.filter .wasSpoken entries
  in
    List.sum (List.map .points spokenEntries)

-- View

title: String -> Int -> Html
title message times =
  message ++ " "
    |> toUpper
    |> repeat times
    |> trimRight
    |> text


pageHeader : Html
pageHeader =
  h1 [ id "logo", class "classy" ] [ title "bingo!" 3 ]

pageFooter: Html
pageFooter =
  footer [ ]
         [ a [ href "https://pragmaticstudio.com",
               target "blank"
             ]
             [text "Prag Studio"]
         ]

entryList : Address Action -> List Entry -> Html
entryList address entries =
  let
    entryItems = List.map (entryItem address) entries
    items = entryItems ++ [totalItem (totalPoints entries)]
  in
    ul [] items

entryItem : Address Action -> Entry -> Html
entryItem address entry =
  li [ classList [ ("highlight", entry.wasSpoken) ],
                 onClick address (Mark entry.id)
     ]
  [ span [ class "phrase" ] [text entry.phrase],
         span [class "points"] [text (toString entry.points)],
              button
              [ class "delete", onClick address (Delete entry.id)]
              [ ]
       ]

entryForm : Address Action -> Model -> Html
entryForm address model =
  div []
      [ input [ type' "text",
               placeholder "Phrase",
               value model.phraseInput,
               autofocus True,
               Utils.onInput address UpdatePhraseInput
              ] [],
        input [ type' "number",
                placeholder "Points",
                value model.pointsInput,
                name "points",
                Utils.onInput address UpdatePointsInput
              ] [],
        button [ class "add",
                 onClick address AddEntry
               ] [ text "Add" ],
        h2 [] [ text (model.phraseInput ++ " " ++ model.pointsInput)]
      ]

totalItem : Int -> Html
totalItem total =
  li [ class "total" ]
     [
      span [class "label" ] [ text "Total" ],
      span [class "points" ] [text (toString total)]
     ]

view : Address Action -> Model -> Html
view address model =
  div [ id "container" ]
        [pageHeader,
         entryForm address model,
         entryList address model.entries,
         button [ class "sort",
                  onClick address Sort
                ]
                [ text "Sort" ],
         pageFooter]

main: Signal Html
main =
  StartApp.start
    { model = initialModel,
      view = view,
      update = update
    }
