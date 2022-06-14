
module Pages.Query exposing (Model, Msg(..), OutMsg(..), init, update, view)

import Url
import Url.Builder as UB
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Element.Font as Font
import Element.Input as I
import Html.Attributes as HA exposing (style)
import Html as H exposing (Html, a, div)

import Maybe.Extra exposing (toList)


type alias Model = 
    { url : String
    , query : String
    , link : Maybe String }


type Msg 
    = QueryChanged String
    | ClearLink
    | GenerateLink
    | TryItOut 


type OutMsg 
    = Goto String
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
        TryItOut -> ( model, Goto model.query )


view : Model -> Element Msg
view model = 
    let emptyQuery = model.query == ""
    in column [ width fill
              , spacingXY 0 20 
              ] 
     <| [ I.search [ width fill
                   , Font.alignLeft
                   , spacingXY 20 0 
                   , centerY
                   ] 
            { onChange = QueryChanged
            , text = model.query
            , placeholder = Nothing
            , label = I.labelHidden "Query" 
            }
        , row [ paddingXY 10 0
              , spacingXY 20 0
              , width fill
              , alignRight 
              ] 
            [ I.button [ alignRight ]
                { onPress = Just <| if emptyQuery then ClearLink else GenerateLink
                , label = text "Generate Link"
                }
            , I.button []
                { onPress = if emptyQuery then Nothing else Just TryItOut
                , label = text "Try It Out"
                }
            ]
        ] ++ Maybe.Extra.toList (Maybe.map (\l -> 
            let params = [ UB.string "q" l ]
            in el [ width fill
                  , clip
                  , htmlAttribute <| HA.style "flex-basis" "auto" 
                  ]
                <| link [ alignLeft ] 
                    { url = UB.absolute [] params
                    , label = text <| model.url ++ UB.relative [] params
                    }
        ) model.link)
