module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Modal as Modal
import GMaps exposing (mapMoved, moveMap)
import Geocoding exposing (..)
import Geolocation exposing (Location)
import Html exposing (Html, button, div, input, p, text)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Http exposing (..)
import Json.Decode as Decode
import List
import SharedModels exposing (GMPos)
import Task
import String exposing (..)


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
    , modalVisibility : Modal.Visibility
    , restaurantResult : Restaurants
    , currentIndex : Int
    }



-- UPDATE


type Msg
    = Update (Result Geolocation.Error Geolocation.Location)
    | MyGeocoderResult (Result Http.Error Geocoding.Response)
    | NewZomatoRequest (Result Http.Error Restaurants)
    | SendGeocodeRequest String
    | Change String
    | GetRestaurant
    | CloseModal
    | ShowModal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Update (Ok location) ->
            let
                newPos =
                    { lat = location.latitude, lng = location.longitude }
            in
            ( { model | pos = newPos, msg = "Automatically Retrived Location" }
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
            ( { model | modalVisibility = Modal.hidden, input = "" }
            , request
            )

        MyGeocoderResult (Ok response) ->
            let
                result =
                    List.head response.results
            in
            case result of
                Just value ->
                    let
                        newPos =
                            { lat = value.geometry.location.latitude
                            , lng = value.geometry.location.longitude
                            }
                    in
                    ( { model
                        | pos = newPos
                        , msg = "Retrieved Location via text input"
                      }
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

        NewZomatoRequest (Ok results) ->
            let
                result =
                    List.head results
            in
                case result of
                    Just result ->
                        let
                            newPos =
                                { lat = Result.withDefault 0 (String.toFloat result.lat)
                                , lng = Result.withDefault 0 (String.toFloat result.lng) }
                        in
                            ( { model | pos = newPos, msg = "Retrieved Suggested Restaurant", restaurantResult = results }
                            , moveMap newPos
                            )

                    Nothing ->
                        ( { model | msg = "Error" }
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

        CloseModal ->
            ( { model | modalVisibility = Modal.hidden, input = "" }, Cmd.none )

        ShowModal ->
            ( { model | modalVisibility = Modal.shown }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ navigationbar model
        , p [] [ text (toString model) ]
        ]


navigationbar : Model -> Html Msg
navigationbar model =
    div [ id "menu-outer" ]
        [ div [ class "table" ]
            [ Html.ul [ id "horizontal-list" ]
                [ Html.li []
                    [ Button.button
                        [ Button.large
                        , Button.outlineSecondary
                        , Button.attrs [ onClick ShowModal ]
                        ]
                        [ text "Change Location" ]
                    ]
                , Html.li []
                    [ Button.button
                        [ Button.large
                        , Button.outlineSecondary
                        , Button.attrs [ onClick GetRestaurant ]
                        ]
                        [ text "Get Restaurant" ]
                    ]
                , Html.li [] [ modal model ]
                ]
            ]
        ]


modal : Model -> Html Msg
modal model =
    Modal.config CloseModal
        |> Modal.large
        |> Modal.body []
            [ Form.form []
                [ Html.h3 [] [ text "You Want To Change Your Location?" ]
                , Html.br [] []
                , Form.group []
                    [ Input.text
                        [ Input.attrs [ placeholder "Enter Your Location Then" ]
                        , Input.onInput Change
                        , Input.value model.input
                        ]
                    ]
                ]
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.outlineSuccess
                , Button.attrs [ onClick (SendGeocodeRequest model.input) ]
                ]
                [ text "Enter" ]
            , Button.button
                [ Button.outlineDanger
                , Button.attrs [ onClick CloseModal ]
                ]
                [ text "Cancel" ]
            ]
        |> Modal.view model.modalVisibility



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Geolocation.changes (Update << Ok)
        ]



-- INIT


initModel : Model
initModel =
    { pos = GMPos 1.292393 103.77572600000008
    , msg = "Trying to get current location.."
    , input = ""
    , modalVisibility = Modal.hidden
    , restaurantResult = []
    , currentIndex = 0
    }


init : ( Model, Cmd Msg )
init =
    ( initModel
    , Task.attempt Update Geolocation.now
    )



-- HTTP


getRestaurant : Float -> Float -> Cmd Msg
getRestaurant lat lng =
    let
        url =
            "https://developers.zomato.com/api/v2.1/search?sort=real_distance&count=10&lat=" ++ toString lat ++ "&lon=" ++ toString lng
    in
    Http.send NewZomatoRequest
        (Http.request
            { method = "GET"
            , headers = [ header "user-key" "cf56a7f076c8d0a24251c6ae612709cf" ]
            , url = url
            , body = Http.emptyBody
            , expect = Http.expectJson decodeZomatoJSON
            , timeout = Nothing
            , withCredentials = False
            }
        )




type alias Restaurant =
    { name : String
    , rating : String
    , lat : String
    , lng : String
    , featured_image : String
    , url : String
    , address : String
    }


type alias Restaurants =
    List Restaurant


decodeRestaurant : Decode.Decoder Restaurant
decodeRestaurant =
    Decode.map7 Restaurant
        (Decode.at [ "restaurant", "name" ] Decode.string)
        (Decode.at [ "restaurant", "user_rating", "aggregate_rating" ] Decode.string)
        (Decode.at [ "restaurant", "location", "latitude" ] Decode.string)
        (Decode.at [ "restaurant", "location", "longitude" ] Decode.string)
        (Decode.at [ "restaurant", "featured_image" ] Decode.string)
        (Decode.at [ "restaurant", "url" ] Decode.string)
        (Decode.at [ "restaurant", "location", "address" ] Decode.string)




decodeRestaurants : Decode.Decoder (List Restaurant)
decodeRestaurants =
    Decode.list decodeRestaurant


decodeZomatoJSON : Decode.Decoder (List Restaurant)
decodeZomatoJSON =
    Decode.at [ "restaurants" ] decodeRestaurants
