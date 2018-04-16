module Main exposing (..)

import Html exposing (Html, div, p, text, button)
import Html.Events exposing (onClick)
import SharedModels exposing (GMPos)
import GMaps exposing (moveMap, mapMoved)
import Geolocation exposing (Location)
import Task


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
    { pos : GMPos
    }



-- UPDATE


type Msg
    = Move Direction
    | MapMoved GMPos
    | Update (Result Geolocation.Error Geolocation.Location)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move direction ->
            let
                newPos =
                    movePos model.pos direction
            in
                ( { model | pos = newPos }
                , moveMap newPos
                )

        MapMoved newPos ->
            ( { model | pos = newPos }
            , Cmd.none
            )

        Update (Ok location) ->
            let
                newPos =
                    { lat = location.latitude, lng = location.longitude }
            in
                ( { model | pos = newPos }
                , moveMap newPos
                )

        Update (Err err) ->
            ( model
            , Cmd.none
            )


type Direction
    = North
    | South
    | West
    | East


movePos : GMPos -> Direction -> GMPos
movePos pos direction =
    case direction of
        North ->
            { pos | lat = pos.lat + 0.001 }

        South ->
            { pos | lat = pos.lat - 0.001 }

        West ->
            { pos | lng = pos.lng - 0.001 }

        East ->
            { pos | lng = pos.lng + 0.001 }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text ("Latitude: " ++ toString model.pos.lat) ]
        , p [] [ text ("Longitude: " ++ toString model.pos.lng) ]
        , button [ onClick (Move North) ] [ text "North" ]
        , button [ onClick (Move South) ] [ text "South" ]
        , button [ onClick (Move West) ] [ text "West" ]
        , button [ onClick (Move East) ] [ text "East" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ mapMoved MapMoved
        , Geolocation.changes (Update << Ok)
        ]



-- INIT


init : ( Model, Cmd Msg )
init =
    ( Model (GMPos 1.292393 103.77572600000008)
    , Task.attempt Update Geolocation.now
    )
