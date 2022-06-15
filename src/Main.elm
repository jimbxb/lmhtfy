port module Main exposing (..)

import Pages.Query    as Q
import Pages.Tutorial as T

import Style as S

import Browser
import Browser.Navigation as Nav
import Url
import Url.Builder as UB
import Url.Parser as UParser exposing (parse, string, (</>))
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




port copy : String -> Cmd msg


init : Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init url key =
    let model = { page = QueryPage <| Q.init (Url.toString url) "" Nothing 
                , key = key
                }
    in case parseUrl url of
        Nothing -> ( model, Nav.pushUrl model.key "lmhtfy" )
        Just Nothing -> ( model, Cmd.none )
        Just (Just q) -> ( { model | page = TutorialPage <| T.init q }, Cmd.none )


parseUrl : Url.Url -> Maybe (Maybe String)
parseUrl url = parse (UParser.s "lmhtfy" </> UParser.query (UQParser.string "q")) url


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
                    Q.Copy id -> copy id
                    Q.Nop -> Cmd.none 
            in ( { model | page = QueryPage newQmodel }, cmd )
        ( TutorialMsg tmsg, TutorialPage tmodel ) -> 
            let ( newTmodel, cmdMsg ) = T.update tmsg tmodel
                cmd = case cmdMsg of
                    T.Goto url -> Nav.pushUrl model.key url
                    T.ExternalGoto url -> Nav.load url
                    T.Nop -> Cmd.none 
            in ( { model | page = TutorialPage newTmodel }, cmd )
        _ -> ( model, Cmd.none )


tutorialLink : String -> String
tutorialLink query = UB.relative [] [ UB.string "q" query ]


view : Model -> Browser.Document Msg
view model =
    let content = case model.page of
            QueryPage m -> Q.view m |> Element.map QueryMsg
            TutorialPage m -> T.view m |> Element.map TutorialMsg
    in 
        { title = title
        , body = [ layout [ width fill
                          , centerX 
                          , Background.color S.lightGrey
                          ] 
                     <| column 
                        [ centerX 
                        , width fill
                        , height fill
                        ]
                        [ topBar
                        , el [ paddingXY 10 20
                             , width (fill |> maximum (800))
                             , centerX
                             , height fill
                             ] content
                        , bottomBar
                        ]
                 ]
        }


topBar = 
    el [ width fill 
       , height (px 60)
       , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
       , Border.color S.black
       , Background.color S.white
       , Border.shadow { blur = 10, color = S.darkGrey, offset = (0, 0), size = 2 }
       ]
     <| row [ width (fill |> maximum (800))
            , paddingXY 10 10
            , height fill
            , centerX
            , centerY
            ]
            [ image [ height (px 40) ]
                { src = "assets/lmhtfy.svg"
                , description = "LMHTFY Logo"
                } 
            ]

bottomBar = 
    el [ width fill 
       , height (px 60)
       , Border.widthEach { bottom = 0, top = 1, left = 0, right = 0 }
       , Border.color S.black
       , Background.color S.white
       , Border.shadow { blur = 10, color = S.darkGrey, offset = (0, 0), size = 2 }
       ]
     <| row [ width (fill |> maximum (800))
            , height fill
            , paddingXY 10 10
            , centerX
            , centerY
            , Font.size 12 
            ]
            [ wrappedRow [ centerX ]
                [ text "© "
                , link [ Font.color S.mediumPurple ] 
                    { url = "https://github.com/jimbxb"
                    , label = text "James Barnes"
                    }
                , text ", 2021. "
                , text "LMHTFY is not endorsed by, sponsored by, or affiliated with "
                , link [ Font.color S.mediumPurple ]
                    { url = "https://www.haskell.org/"
                    , label = text "Haskell.org"
                    }
                , text " nor "
                , link [ Font.color S.mediumPurple ]
                    { url = "https://hoogle.haskell.org/"
                    , label = text "Hoogle"
                    }
                , text "."
                ]
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
