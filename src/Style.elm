module Style exposing (..)

-- TODO: add animations
-- import Simple.Animation.Property as P
-- import Simple.Transition as T
-- import Simple.Animation exposing (Animation)
-- import Simple.Animation.Animated as Animated

import Element as E
import Element.Background as Bg
import Element.Border as B
import Element.Font as F
import Element.Input as I
import Html.Attributes as HA
import Utils exposing (..)


darkPurple : E.Color
darkPurple =
    E.rgb255 69 58 98


mediumPurple : E.Color
mediumPurple =
    E.rgb255 94 80 134


lightPurple : E.Color
lightPurple =
    E.rgb255 143 78 139


black : E.Color
black =
    E.rgb255 0 0 0


darkGrey : E.Color
darkGrey =
    E.rgb255 50 50 50


grey : E.Color
grey =
    E.rgb255 69 58 98


lightGrey : E.Color
lightGrey =
    E.rgb255 190 190 190


white : E.Color
white =
    E.rgb255 255 255 255


checkbox checked =
    E.el
        [ E.width <| E.px 14
        , E.height <| E.px 14
        , E.centerY
        , F.size 9
        , F.center
        , B.rounded 3
        , B.color <|
            if checked then
                darkPurple

            else
                lightGrey
        , Bg.color <|
            if checked then
                darkPurple

            else
                white
        , B.width 1
        , E.inFront
            (E.el
                [ F.color white
                , E.height <| E.px 6
                , E.width <| E.px 9
                , E.rotate <| degrees -45
                , E.centerX
                , E.centerY
                , E.moveUp 1
                , E.transparent <| not checked
                , B.widthEach
                    { top = 0
                    , left = 2
                    , bottom = 2
                    , right = 0
                    }
                ]
                E.none
            )
        ]
        E.none


button :
    Bool
    -> List (E.Attribute msg)
    -> { label : E.Element msg, onPress : Maybe msg }
    -> E.Element msg
button enabled attrs { label, onPress } =
    let
        sty =
            (appendIf enabled
                [ Bg.color mediumPurple
                , B.color darkPurple
                , E.mouseOver
                    [ E.alpha 1
                    , E.moveUp 2
                    , B.shadow { shadow | offset = ( 7, 7 ) }
                    ]
                ]
             <|
                [ F.color white
                , E.paddingXY 15 15
                , B.rounded 10
                , B.width 3
                , E.alpha 0.9
                , Bg.color darkGrey
                , B.color black
                , B.shadow shadow
                , E.focused
                    [ E.alpha 1
                    , E.moveUp 0
                    , B.shadow shadow
                    ]
                ]
            )
                ++ attrs
    in
    if enabled then
        I.button sty { label = label, onPress = onPress }

    else
        E.el sty label


text : List (E.Attribute msg) -> List (E.Element msg) -> E.Element msg
text attrs =
    E.row <| textAttrs ++ attrs


textAttrs : List (E.Attribute msg)
textAttrs =
    [ E.width E.fill
    , E.paddingXY 20 20
    , F.color darkPurple
    , F.alignLeft
    , Bg.color white
    , B.rounded 10
    , B.width 0
    , B.shadow shadow
    ]


querySearch : List (E.Attribute msg) -> { onChange : String -> msg, query : String } -> E.Element msg
querySearch attrs { onChange, query } =
    I.search (textAttrs ++ E.focused [] :: attrs)
        { onChange = onChange
        , text = query
        , placeholder = Just <| I.placeholder [] <| E.text "Search for..."
        , label = I.labelHidden "Query"
        }


link : { url : String, label : E.Element msg } -> E.Element msg
link =
    E.link [ F.color mediumPurple ]


shadow : { blur : Float, color : E.Color, offset : ( Float, Float ), size : Float }
shadow =
    { blur = 10, color = darkGrey, offset = ( 5, 5 ), size = -2 }


clipped : List (E.Attribute msg)
clipped =
    [ E.clip
    , E.htmlAttribute <| HA.style "flex-basis" "auto"
    ]



-- animated :
--     Animation
--     -> (List (E.Attribute msg) -> child -> E.Element msg)
--     -> List (E.Attribute msg)
--     -> child
--     -> E.Element msg
-- animated =
--     flip <|
--         Animated.ui
--             { behindContent = E.behindContent
--             , htmlAttribute = E.htmlAttribute
--             , html = E.html
--             }
