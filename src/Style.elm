module Style exposing (..)

-- import Simple.Animation.Property as P
-- import Simple.Transition as T

import Colors.Opaque as C exposing (..)
import Element as E
import Element.Background as Bg
import Element.Border as B
import Element.Font as F
import Element.HexColor as H
import Element.Input as I
import Simple.Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Utils exposing (..)


darkPurple : E.Color
darkPurple =
    Maybe.withDefault C.black <| H.hex "453a62"


mediumPurple : E.Color
mediumPurple =
    Maybe.withDefault C.black <| H.hex "5e5086"


lightPurple : E.Color
lightPurple =
    Maybe.withDefault C.black <| H.hex "8f4e8b"


darkGrey : E.Color
darkGrey =
    Maybe.withDefault C.black <| H.hex "323232"


lightGrey : E.Color
lightGrey =
    Maybe.withDefault C.black <| H.hex "bebebe"


black : E.Color
black =
    C.black


white : E.Color
white =
    C.white


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
                    , B.shadow
                        { blur = 10
                        , color = darkGrey
                        , offset = ( 7, 7 )
                        , size = -2
                        }
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
                , B.shadow
                    { blur = 10
                    , color = darkGrey
                    , offset = ( 5, 5 )
                    , size = -2
                    }
                ]
            )
                ++ E.focused
                    [ E.moveUp 0
                    , B.shadow
                        { blur = 10
                        , color = darkGrey
                        , offset = ( 7, 7 )
                        , size = -2
                        }
                    ]
                :: attrs
    in
    if enabled then
        I.button sty { label = label, onPress = onPress }

    else
        E.el sty label


text : List (E.Attribute msg) -> List (E.Element msg) -> E.Element msg
text attrs =
    E.row <| textStyle ++ attrs


textStyle : List (E.Attribute msg)
textStyle =
    [ F.color darkPurple
    , F.alignLeft
    , Bg.color white
    , E.paddingXY 20 20
    , E.width E.fill
    , B.rounded 10
    , B.shadow { blur = 10, color = darkGrey, offset = ( 5, 5 ), size = -2 }
    ]


querySearch : List (E.Attribute msg) -> { onChange : String -> msg, query : String } -> E.Element msg
querySearch attrs { onChange, query } =
    I.search (textStyle ++ E.focused [] :: attrs)
        { onChange = onChange
        , text = query
        , placeholder =
            Just <|
                I.placeholder [] <|
                    E.text "Enter a query..."
        , label = I.labelHidden "Query"
        }


link : { url : String, label : E.Element msg } -> E.Element msg
link =
    E.link [ F.color mediumPurple ]


animated :
    Animation
    -> (List (E.Attribute msg) -> child -> E.Element msg)
    -> List (E.Attribute msg)
    -> child
    -> E.Element msg
animated =
    flip <|
        Animated.ui
            { behindContent = E.behindContent
            , htmlAttribute = E.htmlAttribute
            , html = E.html
            }
