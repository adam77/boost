module Main exposing (..)

import Basics exposing (always)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (zip)
import Test exposing (..)
import Expect


--
-- A simple cipher
--


constant : String
constant =
    "abcdefghijklmnopqrstuvwxyz"


cipher : String
cipher =
    "oephjizkxdawubnytvfglqsrcm"


decode : String -> String
decode encoded =
    let
        substitution =
            String.toList constant
                |> zip (String.toList cipher)
                |> Dict.fromList

        replace char =
            case Dict.get char substitution of
                Just newChar ->
                    newChar

                Nothing ->
                    char
    in
        String.toList encoded
            |> List.map replace
            |> String.fromList



--
-- Simple application to demonstrate the cipher
--


type alias Model =
    { text : String
    }


type Msg
    = TextChanged String


main : Program Never Model Msg
main =
    program
        { init = ( { text = "knlfgnb, sj koqj o yvnewju" }, Cmd.none )
        , view = view
        , update = update
        , subscriptions = always (Sub.batch [])
        }


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text "Cipher text: "
            , input
                [ value model.text
                , onInput TextChanged
                ]
                []
            ]
        , div
            []
            [ text "Plain text: "
            , text <| decode model.text
            ]
        ]


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        TextChanged newText ->
            ( { model | text = newText }, Cmd.none )



--
-- tests
--


suite : Test
suite =
    describe "The cipher module"
        [ describe "decode"
            [ test "the decoder works on a known input" <|
                \_ ->
                    let
                        decoded =
                            decode "knlfgnb, sj koqj o yvnewju"
                    in
                        Expect.equal decoded "houston, we have a problem"
            ]
        ]
