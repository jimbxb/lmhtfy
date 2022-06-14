module Pages.Tutorial exposing (Model, Msg(..), OutMsg(..), init, update, view)


import Url.Builder as UB
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input

type alias Model = 
    { query : String }

type Msg
    = Replay
    | Query
    | HoogleHome
    | HoogleIt

type OutMsg 
    = Goto String
    | Nop

init : String -> Model
init q = { query = q }

update : Msg -> Model -> ( Model, OutMsg )
update msg model = 
    case msg of
        Replay -> ( model, Nop )
        Query -> ( model, Goto "/" )
        HoogleHome -> ( model, Goto hoogleHome )
        HoogleIt -> ( model, Goto <| hoogleQuery model.query )
        


view : Model -> Element Msg
view model = 
    column [ spacingXY 0 20, width fill, clip ] 
     <| List.indexedMap (\i x -> row [] [text <| String.fromInt (i + 1) ++ ". ", x])
            [ text "Visit hoogle.haskell.com"
            , text <| "Search for your query: " ++ model.query
            , text "Click Search"
            ]
        ++ [ row [ spacingXY 20 0, width fill ] 
                [ Input.button [ alignRight ] 
                    { onPress = Just HoogleHome
                    , label = text "Try It Out"
                    }
                , Input.button []
                    { onPress = Just HoogleIt
                    , label = text "Hoogle It"
                    }
                , Input.button []
                    { onPress = Just Query
                    , label = text "Try Another"
                    }
                ]
        ]


hoogle : List UB.QueryParameter -> String
hoogle qs = UB.crossOrigin "https://hoogle.haskell.org" [] qs

hoogleHome : String
hoogleHome = hoogle []

hoogleQuery : String -> String
hoogleQuery q = hoogle [ UB.string "hoogle" q ]

