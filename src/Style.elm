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


c =
    { darkPurple = E.rgb255 69 58 98 -- 453a62
    , mediumPurple = E.rgb255 94 80 134 -- 5e5086
    , lightPurple = E.rgb255 143 78 139 -- 8f4e8b
    , black = E.rgb255 0 0 0
    , darkGrey = E.rgb255 50 50 50 -- 323232
    , lightGrey = E.rgb255 190 190 190 -- bebebe
    , white = E.rgb255 255 255 255
    }


button :
    Bool
    -> List (E.Attribute msg)
    -> { label : E.Element msg, onPress : Maybe msg }
    -> E.Element msg
button enabled attrs { label, onPress } =
    let
        sty =
            (appendIf enabled
                [ Bg.color c.mediumPurple
                , B.color c.darkPurple
                , E.mouseOver
                    [ E.alpha 1
                    , E.moveUp 2
                    , B.shadow { shadow | offset = ( 7, 7 ) }
                    ]
                ]
             <|
                [ F.color c.white
                , E.paddingXY 15 15
                , B.rounded 10
                , B.width 3
                , E.alpha 0.9
                , Bg.color c.darkGrey
                , B.color c.black
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
    , F.color c.darkPurple
    , F.alignLeft
    , Bg.color c.white
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
    E.link [ F.color c.mediumPurple ]


shadow : { blur : Float, color : E.Color, offset : ( Float, Float ), size : Float }
shadow =
    { blur = 10, color = c.darkGrey, offset = ( 5, 5 ), size = -2 }


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
