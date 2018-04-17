module Main exposing (..)

import Bootstrap.Button as Button
import GMaps exposing (mapMoved, moveMap)
import Geocoding exposing (..)
import Geolocation exposing (Location)
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import SharedModels exposing (GMPos)
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
    , changeLocationRequest : Bool
    , apiKey : String
    }



-- UPDATE


type Msg
    = Update (Result Geolocation.Error Geolocation.Location)
    | ChangeLocation


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        ChangeLocation ->
            ( { model | changeLocationRequest = True }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text ("Message: " ++ model.msg) ]
        , p [] [ text ("Latitude: " ++ toString model.pos.lat) ]
        , p [] [ text ("Longitude: " ++ toString model.pos.lng) ]
        , Html.hr [] []
        , div [ class "test" ] [ navigationBar model ]
        ]


navigationBar : Model -> Html Msg
navigationBar model =
    div [ class "navigationbar" ]
        [ Button.button
            [ Button.outlineDark
            , Button.attrs [ onClick ChangeLocation ]
            ]
            [ text "Change Location" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Geolocation.changes (Update << Ok)



-- INIT


initModel : Model
initModel =
    { pos = GMPos 1.292393 103.77572600000008
    , msg = "Trying to get current location.."
    , changeLocationRequest = False
    , apiKey = "AIzaSyDxfLP4PFyN4hmxKOc0l2xq_tMswDctRAA"
    }


init : ( Model, Cmd Msg )
init =
    ( initModel
    , Task.attempt Update Geolocation.now
    )
