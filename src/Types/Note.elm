module Types.Note exposing
    ( CreationData
    , ReadData
    , decoder
    , decoderList
    , encode
    , new
    )

import Iso8601
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode
import Time exposing (Posix)
import Types.Domain as Domain exposing (Domain)
import Types.Specialty as Specialty exposing (Specialty)
import Types.User as User exposing (User)
import Types.YearLevel as YearLevel exposing (YearLevel)



-- TODO decode comments


type alias CreationData =
    { yearLevel : YearLevel
    , specialty : Specialty
    , domain : Domain
    , title : String
    , content : String
    }


new : CreationData
new =
    { yearLevel = YearLevel.YearNone
    , specialty = Specialty.SpecialtyNone
    , domain = Domain.DomainNone
    , title = ""
    , content = ""
    }


type alias ReadData =
    { yearLevel : YearLevel
    , specialty : Specialty
    , domain : Domain
    , title : String
    , content : String
    , contributor : User
    , createdAt : Posix
    , modifiedAt : Posix
    }


encode : CreationData -> Value
encode data =
    Encode.object
        [ ( "year_level", YearLevel.encode data.yearLevel )
        , ( "specialty", Specialty.encode data.specialty )
        , ( "domain", Domain.encode data.domain )
        , ( "title", Encode.string data.title )
        , ( "content", Encode.string data.content )
        ]


decoder : Decoder ReadData
decoder =
    Decode.succeed ReadData
        |> required "year_level" YearLevel.decoder
        |> required "specialty" Specialty.decoder
        |> required "domain" Domain.decoder
        |> required "title" Decode.string
        |> required "content" Decode.string
        |> required "contributor" User.decoder
        |> required "created_at" Iso8601.decoder
        |> required "modified_at" Iso8601.decoder


decoderList : Decoder (List ReadData)
decoderList =
    Decode.field "results" (Decode.list decoder)
