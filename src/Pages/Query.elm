
module Pages.Query exposing (Model, Msg(..), OutMsg(..), init, update, view)

import Style as S

import Url
import Url.Builder as UB
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Element.Font as Font
import Element.Input as I
import Html.Attributes as HA exposing (style, id)

import Maybe.Extra exposing (toList)


type alias Model = 
    { url : String
    , query : String
    , link : Maybe String }


type Msg 
    = QueryChanged String
    | ClearLink
    | GenerateLink
    | CopyLink String
    | TryItOut 


type OutMsg 
    = Goto String
    | Copy String
    | Nop


init : String -> String -> Maybe String -> Model
init url query link = 
    { url = url
    , query = query
    , link = link 
    }


update : Msg -> Model -> ( Model, OutMsg )
update msg model = 
    case msg of
        QueryChanged q -> ( { model | query = q }, Nop )
        ClearLink -> ( { model | link = Nothing }, Nop )
        GenerateLink -> ( { model | link = Just <| model.query }, Nop )
        CopyLink id -> ( model, Copy id )
        TryItOut -> ( model, Goto model.query )


view : Model -> Element Msg
view model = 
    let emptyQuery = model.query == ""
    in column [ spacingXY 0 20
           , width fill
           ] 
     <| [el [ spacingXY 0 10
                , paddingXY 10 0
                , width fill
                ]
         <| I.search S.textStyle
                { onChange = QueryChanged
                , text = model.query
                , placeholder = Just <| I.placeholder [] 
                                     <| text "Enter a query..."
                , label = I.labelHidden "Query" 
                }
        , wrappedRow [ width fill
                     , spacingXY 20 20
                     , paddingXY 20 0 
                     , alignRight
                     ]  
         <| List.map (S.button [ alignRight ])
         <| Maybe.Extra.toList (Maybe.map (
                always 
                 <| { onPress = Just (CopyLink "link")
                    , label = text "Copy Link"
                    }
            ) model.link)
             ++ 
            [ { onPress = Just <| if emptyQuery then ClearLink else GenerateLink
              , label = text "Generate Link"
              }
            , { onPress = if emptyQuery then Nothing else Just TryItOut
              , label = text "Try It Out"
              }
            ]
        ] ++ Maybe.Extra.toList (Maybe.map (\q -> 
            let params = [ UB.string "q" q ]
                href = model.url ++ UB.relative [] params
            in S.text [ clip
                      , htmlAttribute <| HA.style "flex-basis" "auto"
                      ]
                    [ link [ alignLeft, htmlAttribute <| id "link" ] 
                        { url = UB.relative [] params
                        , label = text <| model.url ++ UB.relative [] params
                        }
                    ]

        ) model.link)
