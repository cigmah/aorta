module Types.Paginated exposing (..)

{-| Represents a paginated object.
-}

import Json.Decode as Decode exposing (Decoder, Value)


type alias Paginated a =
    { next : Maybe Int -- next page number
    , previous : Maybe Int -- previous page number
    , count : Int -- total number of entries
    , results : List a -- entries for this page
    }


decoder : Decoder a -> Decoder (Paginated a)
decoder aDecoder =
    Decode.map4 Paginated
        (Decode.field "next" (Decode.maybe Decode.int))
        (Decode.field "previous" (Decode.maybe Decode.int))
        (Decode.field "count" Decode.int)
        (Decode.field "results" (Decode.list aDecoder))
