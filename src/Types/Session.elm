port module Types.Session exposing
    ( Session
    , addMessage
    , clearMessages
    , decoder
    , default
    , save
    )

import Browser.Navigation exposing (Key)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Types.Credentials as Credentials exposing (..)
import Types.Domain as Domain exposing (Domain)
import Types.Specialty as Specialty exposing (Specialty)
import Types.Test as Test exposing (Test)
import Types.Topic as Topic exposing (Topic)
import Types.YearLevel as YearLevel exposing (YearLevel)


type alias Session =
    { message : Maybe (List String)
    , auth : Auth
    , key : Key
    , reviseTopic : Maybe Topic
    , reviseSpecialty : Maybe Specialty
    , reviseYearLevel : Maybe YearLevel
    , reviseDomain : Maybe Domain
    , test : Maybe Test
    }


fillKey : Auth -> Maybe Topic -> Maybe Specialty -> Maybe YearLevel -> Maybe Domain -> Key -> Session
fillKey auth topic specialty yearLevel domain key =
    { message = Nothing
    , auth = auth
    , key = key
    , reviseTopic = topic
    , reviseSpecialty = specialty
    , reviseYearLevel = yearLevel
    , reviseDomain = domain
    , test = Nothing
    }


default : Key -> Session
default key =
    { message = Nothing
    , auth = Guest
    , key = key
    , reviseTopic = Nothing
    , reviseSpecialty = Nothing
    , reviseYearLevel = Nothing
    , reviseDomain = Nothing
    , test = Nothing
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


encodeMaybe : (a -> Value) -> Maybe a -> Value
encodeMaybe encoder maybeA =
    case maybeA of
        Just a ->
            encoder a

        Nothing ->
            Encode.null


encode : Session -> Value
encode session =
    Encode.object
        [ ( "auth", Credentials.encode session.auth )
        , ( "revise_topic", encodeMaybe Topic.encode session.reviseTopic )
        , ( "revise_specialty", encodeMaybe Specialty.encode session.reviseSpecialty )
        , ( "revise_year_level", encodeMaybe YearLevel.encode session.reviseYearLevel )
        , ( "revise_domain", encodeMaybe Domain.encode session.reviseDomain )
        ]


decoder : Decoder (Key -> Session)
decoder =
    Decode.map5 fillKey
        (Decode.field "auth" Credentials.decoder)
        (Decode.field "revise_topic" <| Decode.maybe <| Topic.decoder)
        (Decode.field "revise_specialty" <| Decode.maybe <| Specialty.decoder)
        (Decode.field "revise_year_level" <| Decode.maybe <| YearLevel.decoder)
        (Decode.field "revise_domain" <| Decode.maybe <| Domain.decoder)



-- Ports


port cache : Value -> Cmd msg


save : Session -> Cmd msg
save session =
    session
        |> encode
        |> cache
