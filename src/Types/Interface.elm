module Types.Interface exposing (..)

{-| This module collects interfaces which can be passed around.

Interfaces functions that are specified for a particular type. They can be
used to write type signatures which use any type that implement functions
provided by an interface.

-}

import Json.Decode exposing (Decoder, Value)


{-| An interface for discriminated unions.
-}
type alias Enumerable a =
    { list : List a
    , count : Int
    , toString : a -> String
    , toBriefString : a -> String
    , toInt : a -> Int
    , fromInt : Int -> a
    , encode : a -> Value
    , decoder : Decoder a
    }
