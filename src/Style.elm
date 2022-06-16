module Style exposing (..)

import Colors.Opaque as C exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.HexColor as H
import Element.Input as I
import Html.Attributes as HA exposing (style)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Simple.Transition as T
import Utils exposing (..)



-- 69  58  98


darkPurple =
    Maybe.withDefault C.black <| H.hex "453a62"



-- 94  80  134


mediumPurple =
    Maybe.withDefault C.black <| H.hex "5e5086"



-- 143 78  139


lightPurple =
    Maybe.withDefault C.black <| H.hex "8f4e8b"



-- 50  50  50


darkGrey =
    Maybe.withDefault C.black <| H.hex "323232"



-- 150 150 150


lightGrey =
    Maybe.withDefault C.black <| H.hex "bebebe"


black =
    C.black


white =
    C.white


button : Bool -> List (Attribute msg) -> { label : Element msg, onPress : Maybe msg } -> Element msg
button enabled attrs { label, onPress } =
    let
        sty =
            (appendIf enabled
                [ Background.color mediumPurple
                , Border.color darkPurple
                , mouseOver
                    [ alpha 1
                    , moveUp 2
                    , Border.shadow
                        { blur = 10
                        , color = darkGrey
                        , offset = ( 7, 7 )
                        , size = -2
                        }
                    ]
                ]
             <|
                [ Font.color white
                , paddingXY 15 15
                , Border.rounded 10
                , Border.width 3
                , alpha 0.9
                , Background.color darkGrey
                , Border.color black
                , Border.shadow
                    { blur = 10
                    , color = darkGrey
                    , offset = ( 5, 5 )
                    , size = -2
                    }
                ]
            )
                ++ attrs
    in
    if enabled then
        I.button sty { label = label, onPress = onPress }

    else
        el sty label


text : List (Attribute msg) -> List (Element msg) -> Element msg
text attrs =
    row <| textStyle ++ attrs


textStyle : List (Attribute msg)
textStyle =
    [ Font.color darkPurple
    , Font.alignLeft
    , Background.color white
    , paddingXY 20 20
    , width fill
    , Border.rounded 10
    , Border.shadow { blur = 10, color = darkGrey, offset = ( 5, 5 ), size = -2 }
    ]


link : { url : String, label : Element msg } -> Element msg
link =
    Element.link [ Font.color mediumPurple ]


animated :
    Animation
    -> (List (Attribute msg) -> child -> Element msg)
    -> List (Attribute msg)
    -> child
    -> Element msg
animated =
    flip <|
        Animated.ui
            { behindContent = behindContent
            , htmlAttribute = htmlAttribute
            , html = html
            }
