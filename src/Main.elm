module Main exposing (main)

-- imports
import Browser
import Html exposing (Html, h1, text)
import Html.Attributes exposing (class, name, type_, for, value)
import Html.Events exposing (onInput)
import Html exposing (input)
import Html exposing (label)
import Html.Attributes exposing (href)
import Html exposing (a)
import Html exposing (section)
import Html exposing (div)
import Html exposing (h2)
import Html exposing (p)
import Html exposing (br)
import Debug

-- Base Model
type alias Model =
    {pageFieldValid : Bool
    , wordFieldValid : Bool
    , page : Float
    , word : Float
    , pageFieldValue : String
    , wordFieldValue : String
    }

-- Initial Model
initialModel : Model
initialModel =
    { pageFieldValid = True
    , wordFieldValid = True
    , page = 1
    , word = 250
    , pageFieldValue = "1"
    , wordFieldValue = "250" }


type Msg
    = PageToWord String
    | WordToPage String

-- Inputs
update : Msg -> Model -> Model
update msg model =
    case msg of
        PageToWord userInput ->
            case String.toFloat userInput of
                Nothing -> 
                    { model
                        | pageFieldValue = userInput
                        , pageFieldValid = False}
                Just number ->
                    { model
                        | page = number
                        , word = pageToWord number
                        , pageFieldValue = userInput
                        , pageFieldValid = True
                        , wordFieldValid = True}

        WordToPage userInput ->
            case String.toFloat userInput of
                Nothing ->
                    { model
                        | wordFieldValid = False
                        , wordFieldValue = userInput}
                Just number ->
                    { model 
                        | word = number
                        , page = wordToPage number
                        , wordFieldValue = userInput
                        , wordFieldValid = True
                        , pageFieldValid = True}
                
-- Conversions
pageToWord page = 
    page * 250.0

wordToPage word =
    word / 250.0

-- HTML
view : Model -> Html Msg
view model =
    section [ class "section" ]
        [ div [ class "container" ]
            [ h1 [ class "title" ] [ text "Book Calculator" ]
            , h2 [class "subtitle"][
                p [][text "Converts the amount of pages to words. Type in numbers in either field and it'll convert the other one for you automatically."]
                , br [][]
            ]
            , label [ for "page", class "label" ] [text "# of Pages:"]
            , input
                (
                  [ name "page"
                  , type_ "text"
                  , onInput PageToWord
                  , value (viewPage model)
                  ]
                  ++
                  getPageFieldValidOrNot model
                )
                []
            , br[][]
            , label [ for "word", class "label" ] [text "# of Words:"]
            , input
                (
                  [ name "word"
                  , type_ "text"
                  , onInput WordToPage
                  , value (viewWord model)
                  ]
                  ++
                  getWordFieldValidOrNot model
                )
                []
            ]
        ]

-- Returns
getPageFieldValidOrNot model =
    if model.pageFieldValid == True then
        [class "input"]
    else
        [class "input is-danger"]


viewPage model =
    if model.pageFieldValid == True then
        model.page |> Debug.toString
    else
        model.pageFieldValue |> Debug.toString

getWordFieldValidOrNot model =
    if model.wordFieldValid == True then
        [class "input"]
    else
        [class "input is-danger"]

viewWord model =
    if model.wordFieldValid == True then
        model.word |> Debug.toString
    else
        model.wordFieldValue |> Debug.toString

-- Main
main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }