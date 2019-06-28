port module Types.Session exposing
    ( Session
    , addMessage
    , changeYearLevel
    , clearMessages
    , decoder
    , default
    , save
    )

import Browser.Navigation exposing (Key)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Types.Credentials as Credentials exposing (..)
import Types.Specialty as Specialty exposing (Specialty)
import Types.YearLevel as YearLevel exposing (YearLevel)


type alias Session =
    { message : Maybe (List String)
    , auth : Auth
    , key : Key
    , yearLevel : YearLevel
    , reviseYearLevel : YearLevel
    , reviseSpecialty : Specialty
    }


fillKey : Auth -> YearLevel -> YearLevel -> Specialty -> Key -> Session
fillKey auth yearLevel reviseYearLevel reviseSpecialty key =
    { message = Nothing
    , auth = auth
    , key = key
    , yearLevel = yearLevel
    , reviseYearLevel = reviseYearLevel
    , reviseSpecialty = reviseSpecialty
    }


default : Key -> Session
default key =
    { message = Nothing
    , auth = Guest
    , key = key
    , yearLevel = YearLevel.Year1
    , reviseYearLevel = YearLevel.Year1
    , reviseSpecialty = Specialty.Anatomy
    }


addMessage : Session -> String -> Session
addMessage session message =
    case session.message of
        Just otherMessages ->
            { session | message = Just (message :: otherMessages) }

        Nothing ->
            { session | message = Just [ message ] }


clearMessages : Session -> Session
clearMessages session =
    { session | message = Nothing }


changeYearLevel : Session -> YearLevel -> Session
changeYearLevel session yearLevel =
    { session | yearLevel = yearLevel }


encode : Session -> Value
encode session =
    Encode.object
        [ ( "auth", Credentials.encode session.auth )
        , ( "year_level", YearLevel.encode session.yearLevel )
        , ( "revise_year_level", YearLevel.encode session.reviseYearLevel )
        , ( "revise_specialty", Specialty.encode session.reviseSpecialty )
        ]


decoder : Decoder (Key -> Session)
decoder =
    Decode.map4 fillKey
        (Decode.field "auth" Credentials.decoder)
        (Decode.field "year_level" YearLevel.decoder)
        (Decode.field "revise_year_level" YearLevel.decoder)
        (Decode.field "revise_specialty" Specialty.decoder)



-- Ports


port cache : Value -> Cmd msg


save : Session -> Cmd msg
save session =
    session
        |> encode
        |> cache
