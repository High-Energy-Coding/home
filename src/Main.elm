module Main exposing (..)

import Animator
import Animator.Css
import Animator.Inline
import Browser
import Date exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Iso8601
import Task
import Time exposing (Weekday(..), Month(..), Posix, Zone, utc)



---- MODEL ----


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    , picAndTime : Animator.Timeline PicAndTime
    , whereGoesCalendar : Animator.Timeline WhereGoesCalendar
    }


type WhereGoesCalendar
    = CalendarOnTop
    | CalendarOnBottom


type Flag
    = Red
    | Blue


type alias LastTopBottomFlip =
    ( Time.Posix, Flag )


type alias LastTopBottomFlipAnimator =
    Animator.Timeline WhereGoesCalendar


type PicAndTime
    = TimeOnRight
    | TimeOnLeft


type alias AbleToSwitch =
    Bool


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.Css.watching
            .whereGoesCalendar
            (\newCalendar model ->
                { model | whereGoesCalendar = newCalendar }
            )
        |> Animator.Css.watching
            .picAndTime
            (\newPicAndTime model ->
                { model | picAndTime = newPicAndTime }
            )


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        Time.utc
        (Time.millisToPosix 0)
        (Animator.init TimeOnRight)
        (Animator.init CalendarOnBottom)
    , Task.perform AdjustTimeZone Time.here
    )



---- UPDATE ----


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | FlipTime Time.Posix
    | FlipCalendar Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            let
                updatedModel =
                    model |> Animator.update newTime animator
            in
            ( { updatedModel | time = newTime }, Cmd.none )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        FlipTime _ ->
            let
                updatedPicAndTime =
                    updatePicAndTime model
            in
            ( { model
                | picAndTime = updatedPicAndTime
              }
            , Cmd.none
            )

        FlipCalendar _ ->
            ( { model
                | whereGoesCalendar = updateWhereGoesCalendar model
              }
            , Cmd.none
            )


updateWhereGoesCalendar model =
    case Animator.current model.whereGoesCalendar of
        CalendarOnBottom ->
            Animator.go Animator.slowly CalendarOnTop model.whereGoesCalendar

        CalendarOnTop ->
            Animator.go Animator.slowly CalendarOnBottom model.whereGoesCalendar


updatePicAndTime model =
    case Animator.current model.picAndTime of
        TimeOnLeft ->
            Animator.go Animator.slowly TimeOnRight model.picAndTime

        TimeOnRight ->
            Animator.go Animator.slowly TimeOnLeft model.picAndTime



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "page-container" ]
        [ div [ class "top-bottom" ]
            (decideDayOrNight model)
        ]


decideDayOrNight model =
    let
        hour =
            Time.toHour model.zone model.time
    in
    if hour > 20 || hour < 5 then
        nightTime model

    else
        daytime model


nightTime model =
    [ div [ class "night" ] [ timeViewNaked model ] ]


daytime model =
    [ Animator.Css.div model.whereGoesCalendar
        [ Animator.Css.transform <|
            \state ->
                case state of
                    CalendarOnBottom ->
                        Animator.Css.xy { x = 0, y = 360 }

                    CalendarOnTop ->
                        Animator.Css.xy { x = 0, y = 0 }
        ]
        [ class "top-container"
        ]
      <|
        topContainer model
    , Animator.Css.div model.whereGoesCalendar
        [ Animator.Css.transform <|
            \state ->
                case state of
                    CalendarOnBottom ->
                        Animator.Css.xy { x = 0, y = -360 }

                    CalendarOnTop ->
                        Animator.Css.xy { x = 0, y = 0 }
        ]
        [ class "bottom-container"
        , style "background-image" """url("ooo2.png")"""
        ]
      <|
        bottomContainer model
    ]


topContainer model =
    [ div [ class "cal-container", aboluteTopOffset model ]
        [ googleCalendarIframe
        ]
    ]


takesTimeandReturnsRow : Date.Date -> Int
takesTimeandReturnsRow date =
    let
        zone =
            Time.utc

        isoString =
            Date.toIsoString date

        time =
            case Iso8601.toTime isoString of
                Result.Ok convertedTime ->
                    convertedTime

                Result.Err reason ->
                    Time.millisToPosix 0
    in
    takesTimeandReturnsRowPrivate zone time


takesTimeandReturnsRowPrivate : Time.Zone -> Time.Posix -> Int
takesTimeandReturnsRowPrivate zone time =
    let
        -- Convert Posix time to Date
        date =
            Date.fromPosix zone time

        -- Get the day of the month
        dayOfMonth =
            Date.day date

        -- Get the first day of the month
        firstDayOfMonth =
            Date.fromCalendarDate (Date.year date) (Date.month date) 1

        -- Get the weekday of the first day of the month
        firstWeekday =
            Date.weekday firstDayOfMonth

        -- Map Weekday to an integer (Monday = 0, ..., Sunday = 6)
        weekdayOffset2 =
            case firstWeekday of
                Mon ->
                    0

                Tue ->
                    1

                Wed ->
                    2

                Thu ->
                    3

                Fri ->
                    4

                Sat ->
                    5

                Sun ->
                    6

        -- Calculate the row number
        row =
            ((weekdayOffset2 + dayOfMonth - 1) // 7) + 1
    in
    row



-- never 5th or 6th row.
--


weekdayOffset : Date.Date -> Int
weekdayOffset date =
    Date.weekdayNumber date


type WhichRow
    = FirstRow
    | SecondRow
    | ThirdRow
    | FourthRow


aboluteTopOffset model =
    let
        rowInteger =
            takesTimeandReturnsRowPrivate model.zone model.time
    in
    case rowInteger of
        1 ->
            style "top" "-16px"

        2 ->
            style "top" "-154px"

        3 ->
            style "top" "-290px"

        _ ->
            style "top" "-358px"


bottomContainer model =
    [ timeView model ]


to12Hour : Int -> Int
to12Hour x =
    if x > 12 then
        x - 12

    else if x == 0 then
        12

    else
        x


timeViewNaked model =
    let
        hour =
            String.fromInt (Time.toHour model.zone model.time |> to12Hour)

        minute =
            String.fromInt (Time.toMinute model.zone model.time) |> String.padLeft 2 '0'
    in
    div
        [ class "night-time"
        ]
        [ div [ class "time" ]
            [ span [] [ text hour ]
            , span [] [ text ":" ]
            , span [] [ text minute ]
            ]
        ]


timeView model =
    let
        hour =
            String.fromInt (Time.toHour model.zone model.time |> to12Hour)

        minute =
            String.fromInt (Time.toMinute model.zone model.time) |> String.padLeft 2 '0'
    in
    div
        [ class "time-container"
        ]
        [ div [ class "time" ]
            [ span [] [ text hour ]
            , span [] [ text ":" ]
            , span [] [ text minute ]
            ]
        ]


moanaWideBottom =
    div [ class "picture bottom-half" ] [ boys ]


boys =
    div [ class "boys", style "background-image" """url("boys4.png")""" ]
        []


moanaWideBottomPrevious =
    div [ class "moana", style "background-image" """url("moana.jpeg")""" ]
        [ div [ class "quote" ]
            [ h1 [] [ text "I will carry you here in my heart," ]
            , h1 [] [ text "you'll remind me." ]
            , h1 [] [ text "That come what may" ]
            , h1 [] [ text "I know the way." ]
            ]
        ]


googleCalendarIframe =
    iframe
        [ id "iframe"
        , src "https://calendar.google.com/calendar/embed?height=600&wkst=1&bgcolor=%23ffffff&ctz=America%2FChicago&showTitle=0&showNav=0&showDate=0&showPrint=0&mode=MONTH&showTabs=0&showTz=0&showCalendars=0&src=Y2E2NGdrZmJrNnEwZnNwb2t1cGhsMWEycDhAZ3JvdXAuY2FsZW5kYXIuZ29vZ2xlLmNvbQ&color=%233F51B5&color=%23D50000"
        , style "border-width" "0"
        , width 1080
        , height 730
        , attribute "frameborder" "0"
        , attribute "scrolling" "no"
        ]
        []


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        fliptime =
            35000
    in
    Sub.batch
        [ animator
            |> Animator.toSubscription Tick model
        , Time.every fliptime FlipTime
        , Time.every (fliptime * 4) FlipCalendar
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
