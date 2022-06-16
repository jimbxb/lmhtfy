module Pages.Tutorial exposing (Model, Msg(..), OutMsg(..), init, update, view)

import Element exposing (..)
import Element.Input as I
import Html.Attributes as HA exposing (style)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Simple.Transition as T
import Style as S
import Url.Builder as UB


type alias Model =
    { query : String
    , stage : Stage
    , searchTerm : String
    }


type Stage
    = VisitHoogle
    | TypeQuery
    | HoogleIt


type Msg
    = LMHTFYQuery
    | HoogleHome
    | HoogleQuery
    | NextStage
    | SearchChanged String


type OutMsg
    = Goto String
    | ExternalGoto String
    | Nop


init : String -> Model
init q =
    { query = q, stage = VisitHoogle, searchTerm = "" }


update : Msg -> Model -> ( Model, OutMsg )
update msg model =
    case msg of
        LMHTFYQuery ->
            ( model, Goto "/" )

        HoogleHome ->
            ( model, ExternalGoto hoogleHome )

        HoogleQuery ->
            ( model, ExternalGoto <| hoogleQuery model.query )

        SearchChanged s ->
            ( { model | searchTerm = s }, Nop )

        NextStage ->
            ( { model
                | stage =
                    case model.stage of
                        VisitHoogle ->
                            TypeQuery

                        TypeQuery ->
                            HoogleIt

                        HoogleIt ->
                            VisitHoogle
                , searchTerm = ""
              }
            , Nop
            )


view : Model -> Element Msg
view model =
    let
        correctSearch =
            model.searchTerm == model.query
    in
    column
        [ spacingXY 0 20
        , width fill
        , height fill
        ]
    <|
        [ column
            [ spacingXY 0 20
            , paddingXY 10 0
            , width fill
            ]
          <|
            case model.stage of
                VisitHoogle ->
                    [ S.text []
                        [ text "1. Visit "
                        , S.link
                            { url = hoogleHome
                            , label = text <| hoogleDomain
                            }
                        ]
                    , el [ paddingXY 10 0, alignRight ] <|
                        S.button True
                            [ alignRight ]
                            { onPress = Just NextStage
                            , label = text "Next"
                            }
                    ]

                TypeQuery ->
                    [ S.text
                        [ clip
                        , htmlAttribute <| HA.style "flex-basis" "auto"
                        ]
                        [ text <| "2. Search for " ++ model.query ]
                    , I.search S.textStyle
                        { onChange = SearchChanged
                        , text = model.searchTerm
                        , placeholder =
                            Just <|
                                I.placeholder [] <|
                                    text "Enter a query..."
                        , label = I.labelHidden "Query"
                        }
                    , el [ paddingXY 10 0, alignRight ] <|
                        S.button correctSearch
                            [ alignRight ]
                            { onPress =
                                if correctSearch then
                                    Just NextStage

                                else
                                    Nothing
                            , label = text "Search"
                            }
                    ]

                HoogleIt ->
                    [ S.text [] [ text "3. That's it. You're done. Hoogle It." ]
                    , wrappedRow
                        [ spacingXY 10 10
                        , paddingXY 10 0
                        , alignRight
                        ]
                        [ S.button True
                            []
                            { onPress = Just HoogleQuery
                            , label = text "Hoogle It"
                            }
                        , S.button True
                            []
                            { onPress = Just NextStage
                            , label = text "Restart"
                            }
                        ]
                    ]
        ]


hoogleDomain : String
hoogleDomain =
    "hoogle.haskell.org"


hoogle : List UB.QueryParameter -> String
hoogle qs =
    UB.crossOrigin ("https://" ++ hoogleDomain) [] qs


hoogleHome : String
hoogleHome =
    hoogle []


hoogleQuery : String -> String
hoogleQuery q =
    hoogle [ UB.string "hoogle" q ]
