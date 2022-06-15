module Pages.Tutorial exposing (Model, Msg(..), OutMsg(..), init, update, view)

import Colors as C

import Url.Builder as UB
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as HA exposing (style)


type alias Model = 
    { query : String }


type Msg
    = Replay
    | Query
    | HoogleHome
    | HoogleIt


type OutMsg 
    = Goto String
    | ExternalGoto String
    | Nop


init : String -> Model
init q = { query = q }


update : Msg -> Model -> ( Model, OutMsg )
update msg model = 
    case msg of
        Replay -> ( model, Nop )
        Query -> ( model, Goto "/" )
        HoogleHome -> ( model, ExternalGoto hoogleHome )
        HoogleIt -> ( model, ExternalGoto <| hoogleQuery model.query )
        

view : Model -> Element Msg
view model = 
    column [ spacingXY 0 20
           , width fill
           ] 
     <| [column [ spacingXY 0 10
                , paddingXY 10 0
                , width fill
                ]
         <| List.indexedMap 
                (\i x -> C.text [ clip
                                , htmlAttribute <| HA.style "flex-basis" "auto" 
                                ] [text <| String.fromInt (i + 1) ++ ". ", x])
            [ row [] 
                [ text "Visit "
                , link [ Font.color C.mediumPurple ] 
                    { url = hoogleHome
                    , label = text <| hoogleDomain
                    }
                ]
            , el [] <| text <| "Type in: " ++ model.query
            , text "Click 'Search'"
            ]]
        ++ [ wrappedRow [ spacingXY 20 20
                        , width fill
                        , paddingXY 20 0 
                        , alignRight
                        ] 
             <| List.map (C.button [ alignRight ]) 
                    [ { onPress = Just HoogleHome
                      , label = text "Try It Out"
                      }
                    , { onPress = Just HoogleIt
                      , label = text "Hoogle It"
                      }
                    , { onPress = Just Query
                      , label = text "Try Another"
                      }
                    ]
        ]


hoogleDomain : String
hoogleDomain = "hoogle.haskell.org"


hoogle : List UB.QueryParameter -> String
hoogle qs = UB.crossOrigin ("http://" ++ hoogleDomain) [] qs


hoogleHome : String
hoogleHome = hoogle []


hoogleQuery : String -> String
hoogleQuery q = hoogle [ UB.string "hoogle" q ]
