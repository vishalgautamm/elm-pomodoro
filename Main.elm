module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Time exposing (Time)
import Css exposing (..)


-- CSS


styles : List Mixin -> Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { time : Time
    , paused : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { time = 25 * Time.minute
      , paused = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time
    | Work
    | FCChat
    | Coffee
    | Play
    | Pause
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            case model.paused of
                True ->
                    ( model, Cmd.none )

                False ->
                    ( { model
                        | time = Basics.max 0 (model.time - Time.second)
                      }
                    , Cmd.none
                    )

        Work ->
            ( { model
                | time = 25 * Time.minute
                , paused = False
              }
            , Cmd.none
            )

        FCChat ->
            ( { model
                | time = 10 * Time.minute
                , paused = False
              }
            , Cmd.none
            )

        Coffee ->
            ( { model
                | time = 5 * Time.minute
                , paused = False
              }
            , Cmd.none
            )

        Play ->
            ( { model | paused = False }
            , Cmd.none
            )

        Pause ->
            ( { model | paused = True }
            , Cmd.none
            )

        Reset ->
            ( { model
                | time = 0
                , paused = True
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ div []
                [ h2
                    [ styles
                        [ boxSizing borderBox
                        , textAlign center
                        , fontSize (px 100)
                        ]
                    ]
                    [ model.time |> revealTime |> Html.text ]
                , br [] []
                , span [] [ Html.text "Time to work" ]
                ]
            , buttonTasks
            , br [] []
            , buttonControls
            ]
        ]


buttonTasks : Html Msg
buttonTasks =
    div []
        [ button [ onClick Work ] [ Html.text "Time for some work" ]
        , button [ onClick FCChat ] [ Html.text "FCChat break" ]
        , button [ onClick Coffee ] [ Html.text "Coffee Brk" ]
        ]


buttonControls : Html Msg
buttonControls =
    div []
        [ button
            [ styles
                [ backgroundColor (rgb 0 116 217) ]
            , onClick Play
            ]
            [ Html.text "Play" ]
        , button
            [ styles
                [ backgroundColor (rgb 255 220 0) ]
            , onClick Pause
            ]
            [ Html.text "Pause" ]
        , button
            [ styles
                [ backgroundColor (rgb 255 92 0) ]
            , onClick Reset
            ]
            [ Html.text "Reset" ]
        ]


revealTime : Time -> String
revealTime time =
    let
        minutes =
            (time / Time.minute)
                |> floor
                |> toString

        seconds =
            Basics.round time
                % Basics.round Time.minute
                // Basics.round Time.second
                |> toString
    in
        minutes ++ ":" ++ seconds



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every Time.second Tick
