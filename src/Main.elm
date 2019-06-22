module Main exposing (main)

import Browser
import Browser.Events exposing (onResize)
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import String exposing (fromInt)
import Url
import Url.Parser exposing (Parser, map, oneOf, parse, top)



---- MAIN -----


main =
    Browser.application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



---- MODEL ----


type alias Flags =
    { width : Int
    , height : Int
    }


type alias Model =
    { device : Device
    , key : Nav.Key
    , currentRoute : Route
    , width : Int
    , height : Int
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        x =
            flags.width

        y =
            flags.height

        currentRoute =
            url |> fromUrlToRoute
    in
    ( { device = Element.classifyDevice flags
      , key = key
      , currentRoute = currentRoute
      , width = x
      , height = y
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = DeviceClassified Device
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DeviceClassified device ->
            ( { model | device = device }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            let
                fromUrl =
                    url |> fromUrlToRoute
            in
            ( { model | currentRoute = fromUrl }, Cmd.none )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    onResize <|
        \width height ->
            DeviceClassified (Element.classifyDevice { width = width, height = height })



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    case model.currentRoute of
        NotFound ->
            { title = "Not Found"
            , body = []
            }

        Home ->
            case model.device.orientation of
                Portrait ->
                    case model.device.class of
                        Phone ->
                            { title = "Ashalb Photography"
                            , body = [ body model ]
                            }

                        Tablet ->
                            { title = "Ashalb Photography"
                            , body = [ body model ]
                            }

                        Desktop ->
                            { title = "Ashalb Photography"
                            , body = [ body model ]
                            }

                        BigDesktop ->
                            { title = "Ashalb Photography"
                            , body = [ body model ]
                            }

                Landscape ->
                    case model.device.class of
                        Phone ->
                            { title = "Ashalb Photography"
                            , body = [ body model ]
                            }

                        Tablet ->
                            { title = "Ashalb Photography"
                            , body = [ body model ]
                            }

                        Desktop ->
                            { title = "Ashalb Photography"
                            , body = [ body model ]
                            }

                        BigDesktop ->
                            { title = "Ashalb Photography"
                            , body = [ body model ]
                            }


body : Model -> Html Msg
body model =
    Element.layout []
        (myRowOfStuff model)


myRowOfStuff : Model -> Element msg
myRowOfStuff model =
    wrappedRow [ centerX ]
        ([ el [ Font.bold, paddingXY 10 30 ] (text "Ash's Photo Journey") ]
            ++ List.map
                (\o -> myElement model o)
                bucketObjects
        )


bucketObjects =
    [ "Dune Buckwheat Point Lobos.jpg"
    , "IMG_3553.jpg"
    , "IMG_7910.jpg"
    , "Parish's Nightshade 5-4.jpg"
    , "Scarlet Milkvetch.jpg"
    , "alabama hills.jpg"
    , "lilly2.jpg"
    ]


myElement : Model -> String -> Element msg
myElement model bucketObjectName =
    let
        queryString =
            "?w=" ++ fromInt model.width

        url =
            "https://ash-photo-portfolio.imgix.net/" ++ bucketObjectName

        totalUrl =
            url ++ queryString
    in
    el
        [ paddingXY 0 30
        ]
        (Element.image [] { src = totalUrl, description = "A flower that looks a little like cotton candy." })


deviceBody : Model -> List (Html Msg)
deviceBody model =
    case model.device.class of
        Phone ->
            mobileLayout

        _ ->
            desktopLayout



---- MOBILE LAYOUT ----


mobileLayout : List (Html Msg)
mobileLayout =
    [ layout []
        (column [ width fill, spacing 20 ]
            []
        )
    ]


desktopLayout : List (Html Msg)
desktopLayout =
    [ layout []
        (column [ width fill ] [])
    ]



---- ROUTER ----


type Route
    = Home
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top ]



---- For links probably


toRoute : String -> Route
toRoute string =
    case Url.fromString string of
        Nothing ->
            NotFound

        Just someUrl ->
            Maybe.withDefault NotFound (parse routeParser someUrl)


fromUrlToRoute : Url.Url -> Route
fromUrlToRoute url =
    case parse routeParser url of
        Nothing ->
            NotFound

        Just r ->
            r
