module Types.Tag exposing
    ( Category(..)
    , Tag
    , TagBase
    , categoryToInt
    , decoder
    , encode
    , intToCategory
    )

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode


type Category
    = Uncategorised
    | YearLevel
    | Specialty
    | Collection
    | Domain
    | Matrix


categoryToInt : Category -> Int
categoryToInt category =
    case category of
        Uncategorised ->
            0

        YearLevel ->
            1

        Specialty ->
            2

        Collection ->
            3

        Domain ->
            4

        Matrix ->
            5


intToCategory : Int -> Category
intToCategory int =
    case int of
        0 ->
            Uncategorised

        1 ->
            YearLevel

        2 ->
            Specialty

        3 ->
            Collection

        4 ->
            Domain

        5 ->
            Matrix

        _ ->
            Uncategorised


{-| Extensible records don't produce a constructor by default, so this is
specified out in full|
-}
type alias Tag =
    { id : Int
    , content : String
    , category : Category
    }


type alias TagBase =
    { content : String
    , category : Category
    }



-- Json


decoder : Decoder Tag
decoder =
    Decode.succeed Tag
        |> required "id" Decode.int
        |> required "content" Decode.string
        |> required "category" (Decode.map intToCategory Decode.int)


encode : TagBase -> Value
encode tag =
    Encode.object
        [ ( "content", Encode.string tag.content )
        , ( "category", Encode.int (categoryToInt tag.category) )
        ]
