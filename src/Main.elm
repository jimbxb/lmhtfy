module Main exposing (..)

import Pages.Query    as Q
import Pages.Tutorial as T

import Browser
import Browser.Navigation as Nav
import Url
import Url.Parser as UParser exposing ((<?>), top, s, map, parse, oneOf)
import Url.Parser.Query as UQParser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Element.Font as Font
import Element.Input as Input
import Colors.Opaque exposing (..)
import Maybe.Extra exposing (toList)
import Url.Builder as UB


---- APP ----


title : String
title = "LMHTFY"


---- MODEL ----


type alias Model = 
    { page : Page
    , key : Nav.Key
    }
    
type Page 
    = QueryPage Q.Model
    | TutorialPage T.Model


init : Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init url key =
    let model = { page = QueryPage <| Q.init (Url.toString url) "" Nothing 
                , key = key
                }
    in case parseUrl url of
        Nothing -> ( model, Nav.pushUrl model.key "/" )
        Just Nothing -> ( model, Cmd.none )
        Just (Just q) -> ( { model | page = TutorialPage <| T.init q }, Cmd.none )
        

parseUrl : Url.Url -> Maybe (Maybe String)
parseUrl url =
    let urlParser = UParser.map identity (UParser.query <| UQParser.string "q")
    in parse urlParser url


---- UPDATE ----


type Msg
    = UrlChanged Url.Url
    | UrlRequest Browser.UrlRequest
    | QueryMsg Q.Msg
    | TutorialMsg T.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( UrlChanged url, _ ) -> init url model.key
        ( UrlRequest req, _ ) -> 
            case req of
                Browser.Internal url -> ( model, Nav.pushUrl model.key <| Url.toString url )
                Browser.External href -> ( model, Nav.load href )
        ( QueryMsg qmsg, QueryPage qmodel ) -> 
            let ( newQmodel, cmdMsg ) = Q.update qmsg qmodel
                cmd = case cmdMsg of
                    Q.Goto q -> Nav.pushUrl model.key <| tutorialLink q
                    Q.Nop -> Cmd.none 
            in ( { model | page = QueryPage newQmodel }, cmd )
        ( TutorialMsg tmsg, TutorialPage tmodel ) -> 
            let ( newTmodel, cmdMsg ) = T.update tmsg tmodel
                cmd = case cmdMsg of
                    T.Goto url -> Nav.pushUrl model.key url
                    T.Nop -> Cmd.none 
            in ( { model | page = TutorialPage newTmodel }, cmd )
        _ -> ( model, Cmd.none )


tutorialLink : String -> String
tutorialLink query = UB.absolute [] [ UB.string "q" query ]


---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    let 
        content = case model.page of
            QueryPage m -> Q.view m |> Element.map QueryMsg
            TutorialPage m -> T.view m |> Element.map TutorialMsg
    in 
        { title = title
        , body = [ 
            layout [ width fill, centerX ] 
                <| column 
                    [ spacingXY 0 20
                    , paddingXY 20 20
                    , centerX 
                    , width (fill |> maximum (600))
                    ]
                    [ navBar
                    , el [ paddingXY 10 10, width fill] content
                    ]
            ]
        }


navBar = 
    row [ width fill
        , paddingXY 10 10
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        , Border.color blue
        , centerX
        ]
        [ el [ alignLeft ] <| text title
        ]

---- PROGRAM ----


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
