module Main exposing (..)

import Geocoding exposing (..)
import Geolocation exposing (Location)
import GMaps exposing (moveMap, mapMoved)
import Html exposing (Html, div, p, text, button, input)
import Html.Events exposing (onClick, onInput, on, targetValue)
import Html.Attributes exposing (..)
import Http exposing (..)
import SharedModels exposing (GMPos)
import Task
import Json.Decode as Decode
import List


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
    , input : String
    }



-- UPDATE


type Msg
    = Update (Result Geolocation.Error Geolocation.Location)
    | MyGeocoderResult (Result Http.Error Geocoding.Response)
    | NewZomatoRequest (Result Http.Error String)
    | SendGeocodeRequest String
    | Change String
    | GetRestaurant


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

        SendGeocodeRequest locationString ->
            let
                request =
                    Geocoding.requestForAddress "AIzaSyAduosinhGUarThepjoSF5_rjRgTQM9h2U" locationString
                        |> Geocoding.send MyGeocoderResult
            in
                ( model
                , request
                )

        MyGeocoderResult (Ok response) ->
            let
                result =
                    List.head (response.results)
            in
                case result of
                    Just value ->
                        let
                            newPos =
                                { lat = value.geometry.location.latitude, lng = value.geometry.location.longitude }
                        in
                            ( { model | pos = newPos, msg = "Retrieved Location via search" }
                            , moveMap newPos
                            )

                    Nothing ->
                        ( { model | msg = "Error" }
                        , Cmd.none
                        )

        MyGeocoderResult (Err err) ->
            ( { model | msg = toString err }
            , Cmd.none
            )

        Change newContent ->
            ( { model | input = newContent }
            , Cmd.none
            )

        NewZomatoRequest (Ok newUrl) ->
            ( { model | msg = toString newUrl }
            , Cmd.none
            )

        NewZomatoRequest (Err err) ->
            ( { model | msg = toString err }
            , Cmd.none
            )

        GetRestaurant ->
            ( model
            , getRestaurant model.pos.lat model.pos.lng
            )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Enter your location", onInput Change, myStyle ] []
        , button [ onClick (SendGeocodeRequest model.input) ] [ text "Get Location" ]
        , button [ onClick (GetRestaurant) ] [ text "Get Restaurant" ]
        , p [] [ text ("Message: " ++ model.msg) ]
        , p [] [ text ("Input: " ++ toString model.input) ]
        , p [] [ text ("Latitude: " ++ toString model.pos.lat) ]
        , p [] [ text ("Longitude: " ++ toString model.pos.lng) ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Geolocation.changes (Update << Ok)



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { pos = GMPos 1.292393 103.77572600000008, msg = "Trying to get current location..", input = "" }
    , Task.attempt Update Geolocation.now
    )



-- CSS


myStyle : Html.Attribute msg
myStyle =
    style
        [ ( "width", "90%" )
        , ( "height", "40px" )
        , ( "padding", "10px 0" )
        , ( "font-size", "2em" )
        , ( "text-align", "center" )
        ]



-- HTTP


getRestaurant : Float -> Float -> Cmd Msg
getRestaurant lat lng =
    let
        url =
            "https://developers.zomato.com/api/v2.1/geocode?lat=" ++ toString lat  ++ "&lon=" ++ toString lng
    in
        Http.send NewZomatoRequest
            (Http.request
                { method = "GET"
                , headers = [header "user-key" "cf56a7f076c8d0a24251c6ae612709cf"]
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectJson decodeZomatoUrl
                , timeout = Nothing
                , withCredentials = False
                }
            )


decodeZomatoUrl : Decode.Decoder String
decodeZomatoUrl =
    Decode.at [ "location", "nearby_restaurants" ] Decode.string
