module Main exposing (main)

-- imports
import Html.Styled as Html
import Browser
import Debug
import Css exposing (..)
import Css.Global
import Html.Attributes exposing (class, name, type_, for, value)
import Html.Styled.Events exposing (onInput)
import Html.Styled.Attributes as Attr
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Utilities as Tw

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

-- Update based on Input
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
view : Model -> Html.Html Msg
view model =
    Html.section [ Attr.css[Tw.bg_white, Tw.content_center]]
        [ Html.div [ Attr.css [Tw.bg_white, Tw.items_center, Tw.align_middle, Tw.justify_center, Tw.content_center]]
            [ Html.h1 [Attr.css [Tw.text_center, Tw.bg_green_400, Tw.text_blue_100]] [ Html.text "Book Calculator" ]
            , Html.h2 [Attr.class "subtitle"][
                Html.p [Attr.css[Tw.text_center]][Html.text "Converts the amount of pages to words. Type in numbers in either field and it'll convert the other one for you automatically."]
                , Html.br [][]
            ]
            , Html.label [ Attr.for "page", Attr.class "label", Attr.css[Tw.justify_center] ] [Html.text "# of Pages:"]
            , Html.input
                (
                  [ Attr.name "page"
                  , Attr.type_ "text"
                  , onInput PageToWord
                  , Attr.value (viewPage model)
                  ]
                  ++
                  getPageFieldValidOrNot model
                )
                []
            , Html.br[][]
            , Html.label [ Attr.for "word", Attr.class "label" ] [Html.text "# of Words:"]
            , Html.input
                (
                  [ Attr.name "word"
                  , Attr.type_ "text"
                  , onInput WordToPage
                  , Attr.value (viewWord model)
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
        [Attr.class "input"]
    else
        [Attr.class "input is-danger"]


viewPage model =
    if model.pageFieldValid == True then
        model.page |> Debug.toString
    else
        model.pageFieldValue |> Debug.toString

getWordFieldValidOrNot model =
    if model.wordFieldValid == True then
        [Attr.class "input"]
    else
        [Attr.class "input is-danger"]

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
        , view = view >> Html.toUnstyled
        , update = update
        }