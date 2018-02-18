module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import BooleanParser exposing (evalString)


main =
    beginnerProgram
        { view = view
        , model = model
        , update = update
        }


type alias Model =
    { input_ : String, result : String }


type Msg
    = Change_input String
    | Submit


model : Model
model =
    { input_ = "", result = "" }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change_input s ->
            { model | input_ = s }

        Submit ->
            { model | result = evalString model.input_ }


descriptions : List (Html Msg)
descriptions =
    let
        convert s =
            String.split "\n" s |> List.map String.trim |> String.join "" |> text
    in
        [ """
          論理演算をするだけのパーサー。True, Falseはそれぞれ\\(T\\), \\(F\\)、否定は\\(!\\)、
          積と和はそれぞれ\\(\\&\\&\\), \\(||\\)に対応している。
          括弧付きの表記も使える。
          """
        ]
            |> List.map convert


examples =
    ul []
        [ li [] [ text "!T \\(\\rightarrow \\neg{True} \\Rightarrow False\\)" ]
        , li [] [ text "T && T \\(\\rightarrow True \\land True \\Rightarrow True \\)" ]
        , li [] [ text "!(T || F) && T \\(\\rightarrow \\neg{(True \\lor False)} \\land True \\Rightarrow False \\)" ]
        ]


view : Model -> Html Msg
view model =
    let
        inputStyle =
            style [ ( "font-size", "20px" ) ]

        buttonStyle =
            style [ ( "margin", "auto 20px" ) ]
    in
        div
            [ style
                [ ( "margin", "100px auto" )
                , ( "max-width", "800px" )
                ]
            ]
            [ Html.form [ onSubmit Submit ]
                [ input [ onInput Change_input, type_ "text", value model.input_, inputStyle, autofocus True ] []
                , button [ type_ "submit", buttonStyle ] [ text "評価" ]
                , text model.result
                ]
            , p [] descriptions
            , h4 [] [ text "examples" ]
            , examples
            , p []
                [ a [ href "https://github.com/m-shaka/boolean-parser-elm", target "_blank" ]
                    [ text "ソースはこちら" ]
                ]
            ]
