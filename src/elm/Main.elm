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
    , msg : String
    }



-- UPDATE


type Msg
    = MapMoved GMPos
    | Update (Result Geolocation.Error Geolocation.Location)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MapMoved newPos ->
            ( { model | pos = newPos, msg = "Map Moved" }
            , Cmd.none
            )

        Update (Ok location) ->
            let
                newPos =
                    { lat = location.latitude, lng = location.longitude }
            in
                ( { model | pos = newPos, msg = "Retrived Location" }
                , moveMap newPos
                )

        Update (Err err) ->
            ( { model | msg = toString err }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text ("Message: " ++ model.msg) ]
        , p [] [ text ("Latitude: " ++ toString model.pos.lat) ]
        , p [] [ text ("Longitude: " ++ toString model.pos.lng) ]
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
    ( { pos = GMPos 1.292393 103.77572600000008, msg = "Initialized" }
    , Task.attempt Update Geolocation.now
    )
