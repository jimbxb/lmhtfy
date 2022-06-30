module Pages.Tutorial exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Nav
import Delay
import Element exposing (..)
import Element.Font as F
import Style as S
import Url.Builder as UB


type alias Model =
    { key : Nav.Key
    , query : String
    , auto : Bool
    , stage : Stage
    , searchQuery : String
    }


type Stage
    = VisitHoogle
    | TypeQuery
    | HoogleIt


type Msg
    = HoogleQuery
    | SearchChanged String
    | NextStage


init : Nav.Key -> String -> Bool -> Model
init key query auto =
    { key = key
    , query = query
    , stage = VisitHoogle
    , searchQuery = ""
    , auto = auto
    }


update : (Msg -> a) -> Msg -> Model -> ( Model, Cmd a )
update tick msg model =
    case msg of
        HoogleQuery ->
            ( model, Nav.load <| hoogleQuery model.query )

        SearchChanged s ->
            ( { model | searchQuery = s }, Cmd.none )

        NextStage ->
            let
                ( nextStage, cmd ) =
                    case model.stage of
                        VisitHoogle ->
                            ( TypeQuery
                            , Delay.sequenceIf model.auto <|
                                (List.map
                                    (\i ->
                                        ( if i == 1 then
                                            1000

                                          else
                                            min 500 <| 5000 // String.length model.query
                                        , tick <| SearchChanged <| String.left i model.query
                                        )
                                    )
                                 <|
                                    List.range 1 <|
                                        String.length model.query
                                )
                            )

                        TypeQuery ->
                            ( HoogleIt, Cmd.none )

                        HoogleIt ->
                            ( VisitHoogle, Cmd.none )
            in
            ( { model | stage = nextStage, searchQuery = "" }, cmd )


view : Model -> Element Msg
view model =
    let
        correctSearch =
            model.searchQuery == model.query
    in
    column
        [ width fill
        , height fill
        , spacingXY 0 20
        ]
    <|
        [ column
            [ width fill
            , spacingXY 0 20
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
                    , el [ alignRight ] <|
                        S.button True
                            [ alignRight ]
                            { onPress = Just NextStage
                            , label = text "Next"
                            }
                    ]

                TypeQuery ->
                    [ S.text S.clipped
                        [ text <| "2. Search for " ++ model.query ]
                    , if model.auto then
                        let
                            ( sty, txt ) =
                                if correctSearch then
                                    ( [], model.query )

                                else if model.searchQuery == "" then
                                    ( [ F.color S.grey ], "..." )

                                else
                                    ( [], model.searchQuery )
                        in
                        S.text (sty ++ S.clipped) [ text txt ]

                      else
                        S.querySearch []
                            { onChange = SearchChanged
                            , query = model.searchQuery
                            }
                    , el [ alignRight ] <|
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
                    [ S.text [] [ text "3. That's it. Hoogle it." ]
                    , wrappedRow
                        [ spacingXY 10 10
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
