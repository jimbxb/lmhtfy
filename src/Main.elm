module Main exposing (..)

import Pages.Query    as Q
import Pages.Tutorial as T

import Colors as C

import Browser
import Browser.Navigation as Nav
import Url
import Url.Builder as UB
import Url.Parser as UParser exposing (parse)
import Url.Parser.Query as UQParser exposing (string)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Colors.Opaque exposing (..)


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
parseUrl url = parse (UParser.query <| UQParser.string "q") url


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


view : Model -> Browser.Document Msg
view model =
    let content = case model.page of
            QueryPage m -> Q.view m |> Element.map QueryMsg
            TutorialPage m -> T.view m |> Element.map TutorialMsg
    in 
        { title = title
        , body = [ layout [ width fill
                          , centerX 
                          , Background.color C.lightGrey
                          ] 
                     <| column 
                        [ centerX 
                        , width fill
                        ]
                        [ navBar
                        , el [ paddingXY 10 20
                             , width (fill |> maximum (800))
                             , centerX
                             ] content
                        ]
                 ]
        }


navBar = 
    el [ width fill 
       , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
       , Border.color C.black
       , Background.color C.white
       , Border.shadow { blur = 10, color = C.darkGrey, offset = (0, 0), size = 2 }
       ]
     <| row [ width (fill |> maximum (800))
            , paddingXY 10 10
            , centerX
            ]
            [ image []
                { src = "images/lmhtfy.svg"
                , description = "LMHTFY Logo"
                } 
            ]


title : String 
title = "LMHTFY"


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
