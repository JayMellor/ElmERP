module Main exposing (main)

import Browser
import Colors exposing (green)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Events
import Http
import Json.Decode as Decode exposing (Decoder, field, int, list, map, map2, string)



-- MODEL


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Model
    = NoOrder String (Maybe String)
    | NewOrder Order
    | LoadingOrder String
    | LoadSuccess Order
    | LoadError Http.Error
    | SavingOrder Order
    | SaveSuccess
    | SaveError Http.Error Order


init : () -> ( Model, Cmd Msg )
init _ =
    ( NoOrder "" Nothing, Cmd.none )



-- getIntros : Cmd Msg
-- getIntros =
--     Http.get
--         { url = "http://127.0.0.1:8887/intros.json"
--         , expect = Http.expectJson IntrosLoaded (list orderDecoder)
--         }
-- ORDER


type alias Order =
    { header : OrderHeader
    , lines : List OrderLine
    }


emptyOrder : Order
emptyOrder =
    { header =
        { customer =
            { firstName = ""
            , surname = ""
            }
        }
    , lines =
        []
    }


orderDecoder : Decoder Order
orderDecoder =
    map2 Order
        (field "header" headerDecoder)
        (field "lines" lineDecoder |> list)



-- ORDER HEADER


type alias OrderHeader =
    { customer : Person
    }


headerDecoder : Decoder OrderHeader
headerDecoder =
    map OrderHeader
        (field "customer" personDecoder)



-- ORDER LINE


type alias OrderLine =
    { product : String
    , quantity : Int
    }


lineDecoder : Decoder OrderLine
lineDecoder =
    map2 OrderLine
        (field "product" string)
        (field "quantity" int)



-- PERSON


type alias Person =
    { firstName : String
    , surname : String
    }


personDecoder : Decoder Person
personDecoder =
    map2 Person
        (field "firstName" string)
        (field "surname" string)



-- UPDATE


type Msg
    = ReferenceInput String
    | ReferenceSubmitted String
    | OrderLoaded (Result Http.Error Order)
    | NewOrderSelected


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        OrderLoaded result ->
            case result of
                Ok order ->
                    ( LoadSuccess order, Cmd.none )

                Err httpError ->
                    ( LoadError httpError, Cmd.none )

        ReferenceInput ref ->
            ( NoOrder ref Nothing, Cmd.none )

        ReferenceSubmitted ref ->
            if ref == "" then
                ( NoOrder ref (Just "Reference cannot be empty"), Cmd.none )

            else
                ( LoadingOrder ref, Cmd.none )

        NewOrderSelected ->
            ( NewOrder emptyOrder, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Document Title"
    , body = content model
    }


content : Model -> List (Html Msg)
content model =
    [ layout [] <|
        case model of
            NoOrder ref message ->
                noOrderContainer ref message

            NewOrder order ->
                orderContainer order

            LoadingOrder ref ->
                text ("Loading order " ++ ref)

            LoadSuccess order ->
                text "loaded"

            LoadError error ->
                error |> displayError |> text

            SavingOrder _ ->
                text "saving"

            SaveSuccess ->
                text "saved"

            SaveError error order ->
                text "error"
    ]


onKeypress : String -> Msg -> Attribute Msg
onKeypress keyName msg =
    htmlAttribute
        (Html.Events.on "keyup"
            (field "key" string
                |> Decode.andThen
                    (\key ->
                        if key == keyName then
                            Decode.succeed msg

                        else
                            Decode.fail "Ignore other keys"
                    )
            )
        )


onEnter : Msg -> Attribute Msg
onEnter =
    onKeypress "Enter"


noOrderContainer : String -> Maybe String -> Element Msg
noOrderContainer ref maybeMessage =
    column [ height fill, width fill ]
        [ row
            [ width fill
            , height <| fill
            , Background.color <| rgb255 92 99 118
            ]
            [ Input.text
                [ centerX
                , Input.focusedOnLoad
                , width
                    (fill
                        |> maximum 500
                    )
                , onEnter (ReferenceSubmitted ref)
                , below
                    (case maybeMessage of
                        Just message ->
                            el
                                [ moveDown 10
                                , Font.color <| rgb255 232 163 151
                                ]
                            <|
                                text message

                        Nothing ->
                            none
                    )
                ]
                { label =
                    Input.labelAbove
                        [ centerX
                        , moveUp 10
                        , Font.color <| rgb255 255 255 255
                        ]
                    <|
                        text "Open an existing order"
                , placeholder = Just (Input.placeholder [] (text "Order reference"))
                , text = ref
                , onChange = ReferenceInput
                }
            ]
        , row
            [ width fill
            , height <| fill
            ]
            [ Input.button
                [ Background.color green
                , centerX
                , Border.width 10
                , Border.color green
                , Border.rounded 5
                , mouseOver
                    [ Border.color <| rgb255 115 240 115
                    , Background.color <| rgb255 115 240 115
                    ]
                ]
                { label = text "+ Create a New Order"
                , onPress = Just NewOrderSelected
                }
            ]
        ]


orderContainer : Order -> Element Msg
orderContainer order =
    column [ height fill, width fill, explain Debug.todo ]
        [ row [ width fill, height <| fillPortion 1 ]
            [ el [] <| text "something"
            ]
        , row
            [ width fill, height <| fillPortion 3 ]
            [ table []
                { data = order.lines
                , columns =
                    [ { header = text "Product"
                      , width = fill
                      , view = \{ product } -> text product
                      }
                    , { header = text "Quantity"
                      , width = fill
                      , view = \{ quantity } -> text <| String.fromInt quantity
                      }
                    ]
                }
            ]
        ]


displayError : Http.Error -> String
displayError httpError =
    case httpError of
        Http.BadUrl url ->
            "Bad url: " ++ url

        Http.Timeout ->
            "Timeout error"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status

        Http.BadBody body ->
            "Bad body: " ++ body
