port module Pages.Query exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Font as F
import Element.Input as I
import Html.Attributes as HA
import Style as S
import Utils exposing (..)


port copy : String -> Cmd msg


type alias Model =
    { url : String
    , key : Nav.Key
    , auto : Bool
    , query : String
    , link : Maybe { query : String, auto : Bool }
    }


type Msg
    = QueryChanged String
    | ToggleAuto Bool
    | ClearLink
    | CreateLink
    | CopyLink String
    | TryItOut


init : String -> Nav.Key -> String -> Bool -> Model
init url key query auto =
    { url = url
    , key = key
    , query = query
    , auto = auto
    , link = Nothing
    }


update : Msg -> Model -> ( Model, Cmd a )
update msg model =
    case msg of
        QueryChanged q ->
            ( { model | query = q }, Cmd.none )

        ToggleAuto t ->
            ( { model | auto = t }, Cmd.none )

        ClearLink ->
            ( { model | link = Nothing }, Cmd.none )

        CreateLink ->
            ( { model | link = Just <| { query = model.query, auto = model.auto } }, Cmd.none )

        CopyLink id ->
            ( model, copy id )

        TryItOut ->
            ( model, Nav.pushUrl model.key <| internalTutorialUrl model.query model.auto )


view : Model -> Element Msg
view model =
    column
        [ width fill
        , spacingXY 0 20
        ]
    <|
        [ S.querySearch [ width fill ]
            { onChange = QueryChanged
            , query = model.query
            }
        , wrappedRow
            [ width fill
            , spacingXY 20 20
            , alignRight
            ]
          <|
            let
                emptyQuery =
                    model.query == ""
            in
            S.text
                [ width shrink
                , alignRight
                ]
                [ I.checkbox [ F.color S.darkPurple ]
                    { onChange = ToggleAuto
                    , icon = S.checkbox
                    , checked = model.auto
                    , label =
                        I.labelRight []
                            (text "Auto?")
                    }
                ]
                :: List.map (S.button (not emptyQuery) [ alignRight ])
                    [ { onPress =
                            Just <|
                                if emptyQuery then
                                    ClearLink

                                else
                                    CreateLink
                      , label = text "Create Link"
                      }
                    , { onPress =
                            if emptyQuery then
                                Nothing

                            else
                                Just TryItOut
                      , label = text "Try It Out"
                      }
                    ]
        ]
            ++ (case model.link of
                    Nothing ->
                        []

                    Just { query, auto } ->
                        let
                            linkID =
                                "link"
                        in
                        [ S.text S.clipped
                            [ link
                                [ alignLeft
                                , htmlAttribute <| HA.id linkID
                                ]
                                { url = internalTutorialUrl query auto
                                , label = text <| externalTutorialUrl model.url query auto
                                }
                            ]
                        , el [ alignRight ] <|
                            S.button True
                                [ alignRight ]
                                { onPress = Just (CopyLink linkID)
                                , label = text "Copy Link"
                                }
                        ]
               )
