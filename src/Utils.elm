module Utils exposing (..)


appendIf : Bool -> List a -> List a -> List a
appendIf cond toAdd result =
    if cond then
        result ++ toAdd

    else
        result


flip : (a -> b -> c) -> (b -> a -> c)
flip f b a =
    f a b
