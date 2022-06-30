module Utils exposing (..)

import Url exposing (Url)
import Url.Builder as UB
import Url.Parser as UP exposing ((<?>))
import Url.Parser.Query as UQP


type alias Params =
    { query : Maybe String
    , auto : Bool
    }


urlParams : Maybe String -> Maybe String -> Params
urlParams mbQuery mbAuto =
    { query = mbQuery
    , auto = isJust mbAuto
    }


parseUrl : Url -> Maybe Params
parseUrl =
    UP.parse <|
        UP.map urlParams <|
            UP.s lmhtfyPath
                <?> UQP.string queryParam
                <?> UQP.string autoParam


lmhtfyPath : String
lmhtfyPath =
    "lmhtfy"


lmhtfyUrl : String
lmhtfyUrl =
    UB.absolute [ lmhtfyPath ] []


queryParam : String
queryParam =
    "query"


autoParam : String
autoParam =
    "auto"


internalTutorialUrl : String -> Bool -> String
internalTutorialUrl query auto =
    UB.relative [] <| tutorialUrlParams query auto


externalTutorialUrl : String -> String -> Bool -> String
externalTutorialUrl url query auto =
    let
        ( url_, params ) =
            case Url.fromString url of
                Just u ->
                    ( Url.toString <| { u | query = Nothing, path = "" }
                    , List.tail <| String.split "/" u.path
                    )

                Nothing ->
                    ( url, Nothing )
    in
    UB.crossOrigin url_ (Maybe.withDefault [ lmhtfyPath ] params) <|
        tutorialUrlParams query auto


tutorialUrlParams : String -> Bool -> List UB.QueryParameter
tutorialUrlParams query auto =
    UB.string queryParam query
        :: (if auto then
                [ UB.string autoParam "" ]

            else
                []
           )


appendIf : Bool -> List a -> List a -> List a
appendIf cond toAdd result =
    if cond then
        result ++ toAdd

    else
        result


flip : (a -> b -> c) -> (b -> a -> c)
flip f b a =
    f a b


isJust : Maybe a -> Bool
isJust mb =
    case mb of
        Just _ ->
            True

        Nothing ->
            False
