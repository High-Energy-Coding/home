module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Task
import Time



---- MODEL ----


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 0)
    , Task.perform AdjustTimeZone Time.here
    )



---- UPDATE ----


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "page-container" ]
        [ div [ class "top-bottom" ]
            [ div [ class "top-container" ]
                [ div [ class "cal-container" ]
                    [ iframe
                        [ id "iframe"
                        , src "https://calendar.google.com/calendar/embed?height=600&wkst=1&bgcolor=%23ffffff&ctz=America%2FChicago&showPrint=0&showTabs=0&showCalendars=0&showTz=0&showDate=0&showNav=0&showTitle=0&src=Y2E2NGdrZmJrNnEwZnNwb2t1cGhsMWEycDhAZ3JvdXAuY2FsZW5kYXIuZ29vZ2xlLmNvbQ&color=%233F51B5"
                        , style "border-width" "0"
                        , width 1080
                        , height 730
                        , attribute "frameborder" "0"
                        , attribute "scrolling" "no"
                        ]
                        []
                    , img [ src "tree.png", class "tree" ] []
                    ]
                ]
            , div [ class "bottom-container" ] [ moanaWideBottom, timeView model ]
            ]
        ]


timeView model =
    let
        hour =
            String.fromInt (Time.toHour model.zone model.time |> modBy 12)

        minute =
            String.fromInt (Time.toMinute model.zone model.time) |> String.padLeft 2 '0'
    in
    div [ class "time-container" ]
        [ div [ class "time" ]
            [ span [] [ text hour ]
            , span [] [ text ":" ]
            , span [] [ text minute ]
            ]
        ]


moanaWideBottom =
    div [ class "moana bottom-half", style "background-image" """url("santa.jpg")""" ]
        []


moanaWideBottomPrevious =
    div [ class "moana bottom-half", style "background-image" """url("moana.jpeg")""" ]
        [ div [ class "quote" ]
            [ h1 [] [ text "I will carry you here in my heart," ]
            , h1 [] [ text "you'll remind me." ]
            , h1 [] [ text "That come what may" ]
            , h1 [] [ text "I know the way." ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
