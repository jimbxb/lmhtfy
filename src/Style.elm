module Style exposing (..)

import Colors.Opaque as C exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.HexColor as H
import Element.Input as I


darkPurple =
    Maybe.withDefault C.black <| H.hex "453a62"



-- 69  58  98


mediumPurple =
    Maybe.withDefault C.black <| H.hex "5e5086"



-- 94  80  134


lightPurple =
    Maybe.withDefault C.black <| H.hex "8f4e8b"



-- 143 78  139


darkGrey =
    Maybe.withDefault C.black <| H.hex "323232"



-- 50  50  50


lightGrey =
    Maybe.withDefault C.black <| H.hex "bebebe"



-- 150 150 150


black =
    C.black


white =
    C.white


button : Bool -> List (Attribute msg) -> { label : Element msg, onPress : Maybe msg } -> Element msg
button enabled attrs =
    I.button
        ([ Background.color <|
            if enabled then
                mediumPurple

            else
                darkGrey
         , Font.color white
         , paddingXY 15 15
         , Border.rounded 10
         , Border.color <|
            if enabled then
                darkPurple

            else
                black
         , Border.width 3
         ]
            ++ attrs
        )


text : List (Attribute msg) -> List (Element msg) -> Element msg
text attrs =
    row (textStyle ++ attrs)


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
