port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Date exposing (Date)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as I
import Pages.Query as Q
import Pages.Tutorial as T
import Style as S
import Task
import Time exposing (Month(..))
import Url exposing (Url)
import Url.Builder as UB
import Url.Parser as UParser exposing ((</>))
import Url.Parser.Query as UQParser


type alias Model =
    { page : Page
    , key : Nav.Key
    , date : Date
    }


type Page
    = QueryPage Q.Model
    | TutorialPage T.Model


port copy : String -> Cmd msg


lmhtfy : String
lmhtfy =
    UB.absolute [ "lmhtfy" ] []


init : Url -> Nav.Key -> ( Model, Cmd Msg )
init url key =
    let
        model =
            { page = QueryPage <| Q.init (Url.toString url) "" Nothing
            , key = key
            , date = Date.fromCalendarDate 2021 Mar 9
            }
    in
    case parseUrl url of
        Nothing ->
            ( model, Nav.pushUrl model.key lmhtfy )

        Just Nothing ->
            ( model, now )

        Just (Just q) ->
            ( { model | page = TutorialPage <| T.init q }, now )


parseUrl : Url -> Maybe (Maybe String)
parseUrl url =
    UParser.parse (UParser.s "lmhtfy" </> UParser.query (UQParser.string "q")) url


type Msg
    = UrlChanged Url
    | UrlRequest Browser.UrlRequest
    | GoHome
    | SetDate Date
    | QueryMsg Q.Msg
    | TutorialMsg T.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( UrlChanged url, _ ) ->
            init url model.key

        ( UrlRequest req, _ ) ->
            case req of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key <| Url.toString url )

                Browser.External href ->
                    ( model, Nav.load href )

        ( GoHome, _ ) ->
            ( model, Nav.pushUrl model.key lmhtfy )

        ( SetDate date, _ ) ->
            ( { model | date = date }, Cmd.none )

        ( QueryMsg qmsg, QueryPage qmodel ) ->
            let
                ( newQmodel, cmdMsg ) =
                    Q.update qmsg qmodel

                cmd =
                    case cmdMsg of
                        Q.Goto q ->
                            Nav.pushUrl model.key <| tutorialLink q

                        Q.Copy id ->
                            copy id

                        Q.Nop ->
                            Cmd.none
            in
            ( { model | page = QueryPage newQmodel }, cmd )

        ( TutorialMsg tmsg, TutorialPage tmodel ) ->
            let
                ( newTmodel, cmdMsg ) =
                    T.update tmsg tmodel

                cmd =
                    case cmdMsg of
                        T.Goto url ->
                            Nav.pushUrl model.key url

                        T.ExternalGoto url ->
                            Nav.load url

                        T.Nop ->
                            Cmd.none
            in
            ( { model | page = TutorialPage newTmodel }, cmd )

        _ ->
            ( model, Cmd.none )


tutorialLink : String -> String
tutorialLink query =
    UB.relative [] [ UB.string "q" query ]


now : Cmd Msg
now =
    Task.perform SetDate Date.today


view : Model -> Browser.Document Msg
view model =
    let
        content =
            case model.page of
                QueryPage m ->
                    Q.view m |> Element.map QueryMsg

                TutorialPage m ->
                    T.view m |> Element.map TutorialMsg
    in
    { title = title
    , body =
        [ layout
            [ width fill
            , centerX
            , Background.color S.lightGrey
            , clip
            ]
          <|
            column
                [ centerX
                , width fill
                , height fill
                ]
                [ topBar model
                , el
                    [ paddingXY 10 20
                    , width (fill |> maximum 800)
                    , centerX
                    , height fill
                    ]
                    content
                , bottomBar model
                ]
        ]
    }


topBar : Model -> Element Msg
topBar _ =
    el
        [ width fill
        , height (px 60)
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        , Border.color S.black
        , Background.color S.white
        , Border.shadow { blur = 10, color = S.darkGrey, offset = ( 0, 0 ), size = 2 }
        ]
    <|
        row
            [ width (fill |> maximum 800)
            , paddingXY 10 10
            , height fill
            , centerX
            , centerY
            ]
            [ I.button []
                { onPress = Just GoHome
                , label =
                    image [ height (px 40) ]
                        { src = "assets/lmhtfy.svg"
                        , description = "LMHTFY Logo"
                        }
                }
            ]


bottomBar : Model -> Element Msg
bottomBar { date } =
    let
        year =
            Date.year date
    in
    el
        [ width fill
        , height (px 60)
        , Border.widthEach { bottom = 0, top = 1, left = 0, right = 0 }
        , Border.color S.black
        , Background.color S.white
        , Border.shadow { blur = 10, color = S.darkGrey, offset = ( 0, 0 ), size = 2 }
        ]
    <|
        row
            [ width (fill |> maximum 800)
            , height fill
            , paddingXY 10 10
            , centerX
            , centerY
            ]
            [ paragraph [ Font.center, Font.size 12 ]
                [ text "© "
                , S.link
                    { url = "https://github.com/jimbxb"
                    , label =
                        text <|
                            if date == Date.fromCalendarDate year Mar 15 then
                                "Jarnes Bames"

                            else
                                "James Barnes"
                    }
                , text <|
                    ", 2022"
                        ++ (if year > 2022 then
                                "-" ++ String.fromInt year ++ ". "

                            else
                                ". "
                           )
                , text "LMHTFY is not endorsed by, sponsored by, or affiliated with "
                , S.link
                    { url = "https://www.haskell.org/"
                    , label = text "Haskell.org"
                    }
                , text " nor "
                , S.link
                    { url = "https://hoogle.haskell.org/"
                    , label = text "Hoogle"
                    }
                , text "."
                ]
            ]


title : String
title =
    "LMHTFY | Let Me Hoogle That For You"


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = always init
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequest
        }
