port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Date exposing (Date)
import Element exposing (..)
import Element.Background as Bg
import Element.Border as B
import Element.Font as F
import Element.Input as I
import Pages.Query as Q
import Pages.Tutorial as T
import Style as S exposing (shadow)
import Task
import Time exposing (Month(..))
import Url exposing (Url)
import Url.Builder as UB
import Url.Parser as UP exposing ((</>))
import Url.Parser.Query as UQP


port copy : String -> Cmd msg


type alias Model =
    { page : Page
    , key : Nav.Key
    , date : Date
    }


type Page
    = Query Q.Model
    | Tutorial T.Model


type Msg
    = UrlChanged Url
    | UrlRequest Browser.UrlRequest
    | GoHome
    | SetDate Date
    | QueryMsg Q.Msg
    | TutorialMsg T.Msg


init : Url -> Nav.Key -> ( Model, Cmd Msg )
init url key =
    let
        model =
            { page = Query <| Q.init (Url.toString url) "" Nothing
            , key = key
            , date = Date.fromCalendarDate 2021 Mar 9
            }
    in
    case parseUrl url of
        Nothing ->
            ( model, Nav.pushUrl model.key lmhtfyUrl )

        Just Nothing ->
            ( model, now )

        Just (Just q) ->
            ( { model | page = Tutorial <| T.init q }, now )


now : Cmd Msg
now =
    Task.perform SetDate Date.today


parseUrl : Url -> Maybe (Maybe String)
parseUrl =
    UP.parse <| UP.s lmhtfyPath </> UP.query (UQP.string queryParam)


lmhtfyPath : String
lmhtfyPath =
    "lmhtfy"


lmhtfyUrl : String
lmhtfyUrl =
    UB.absolute [ "lmhtfy" ] []


queryParam : String
queryParam =
    "q"


tutorialUrl : String -> String
tutorialUrl query =
    UB.relative [] [ UB.string queryParam query ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( UrlChanged url, _ ) ->
            init url model.key

        ( UrlRequest req, _ ) ->
            ( model
            , case req of
                Browser.Internal url ->
                    Nav.pushUrl model.key <| Url.toString url

                Browser.External href ->
                    Nav.load href
            )

        ( GoHome, _ ) ->
            ( model, Nav.pushUrl model.key lmhtfyUrl )

        ( SetDate date, _ ) ->
            ( { model | date = date }, Cmd.none )

        ( QueryMsg qmsg, Query qmodel ) ->
            let
                ( newQmodel, cmdMsg ) =
                    Q.update qmsg qmodel
            in
            ( { model | page = Query newQmodel }
            , case cmdMsg of
                Q.Goto q ->
                    Nav.pushUrl model.key <| tutorialUrl q

                Q.Copy id ->
                    copy id

                Q.Nop ->
                    Cmd.none
            )

        ( TutorialMsg tmsg, Tutorial tmodel ) ->
            let
                ( newTmodel, cmdMsg ) =
                    T.update tmsg tmodel
            in
            ( { model | page = Tutorial newTmodel }
            , case cmdMsg of
                T.Goto url ->
                    Nav.load url

                T.Nop ->
                    Cmd.none
            )

        _ ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    let
        ( content, title ) =
            case model.page of
                Query m ->
                    ( Q.view m |> Element.map QueryMsg
                    , "LMHTFY | Let Me Hoogle That For You"
                    )

                Tutorial m ->
                    ( T.view m |> Element.map TutorialMsg
                    , "LMHTFY | " ++ m.query
                    )

        year =
            Date.year model.date

        yearRange =
            if year > 2022 then
                "2022-" ++ String.fromInt year

            else
                "2022"

        name =
            if model.date == Date.fromCalendarDate year Mar 15 then
                "Jarnes Bames"

            else
                "James Barnes"

        bar =
            el
                [ width fill
                , height <| px 60
                , B.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                , B.color S.c.black
                , Bg.color S.c.white
                , B.shadow { shadow | offset = ( 0, 0 ) }
                ]

        head =
            row
                [ width <| maximum 800 <| fill
                , height fill
                , paddingXY 10 10
                , centerX
                , centerY
                ]
                [ I.button [ focused [] ]
                    { onPress = Just GoHome
                    , label =
                        image [ height <| px 40 ]
                            { src = "assets/lmhtfy.svg"
                            , description = "LMHTFY Logo"
                            }
                    }
                ]

        foot =
            row
                [ width <| maximum 800 <| fill
                , height fill
                , paddingXY 10 10
                , centerX
                , centerY
                ]
                [ paragraph
                    [ F.center
                    , F.size 12
                    ]
                    [ text "© "
                    , S.link { url = "https://www.github.com/jimbxb/", label = text name }
                    , text <| ", " ++ yearRange ++ ". "
                    , text "LMHTFY is not endorsed by, sponsored by, or affiliated with "
                    , S.link { url = "https://www.haskell.org/", label = text "Haskell.org" }
                    , text " nor "
                    , S.link { url = "https://hoogle.haskell.org/", label = text "Hoogle" }
                    , text "."
                    ]
                ]
    in
    { title = title
    , body =
        [ layout
            [ width fill
            , Bg.color S.c.lightGrey
            , clip
            ]
          <|
            column
                [ width fill
                , height fill
                ]
                [ bar head
                , el
                    [ width <| maximum 800 <| fill
                    , height fill
                    , paddingXY 10 20
                    , centerX
                    ]
                    content
                , bar foot
                ]
        ]
    }


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
