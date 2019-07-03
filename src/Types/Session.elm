port module Types.Session exposing
    ( Session
    , addMessage
    , clearMessage
    , decoder
    , default
    , isGuest
    , save
    )

import Browser.Navigation exposing (Key)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import List.Extra exposing (remove)
import RemoteData exposing (RemoteData(..), WebData)
import Types.Credentials as Credentials exposing (..)
import Types.Domain as Domain exposing (Domain)
import Types.Note as Note
import Types.Specialty as Specialty exposing (Specialty)
import Types.Test as Test exposing (Test)
import Types.Topic as Topic exposing (Topic)
import Types.YearLevel as YearLevel exposing (YearLevel)


type alias Session =
    { message : Maybe (List String)
    , auth : Auth
    , key : Key
    , webDataNoteList : WebData (List Note.ListData)
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
    , webDataNoteList = Loading
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
    , webDataNoteList = Loading
    , reviseTopic = Nothing
    , reviseSpecialty = Nothing
    , reviseYearLevel = Nothing
    , reviseDomain = Nothing
    , test = Nothing
    }


isGuest : Session -> Bool
isGuest session =
    case session.auth of
        Guest ->
            True

        User _ ->
            False


addMessage : Session -> String -> Session
addMessage session message =
    case session.message of
        Just otherMessages ->
            { session | message = Just (message :: otherMessages) }

        Nothing ->
            { session | message = Just [ message ] }


clearMessage : String -> Session -> Session
clearMessage message session =
    case session.message of
        Just stringList ->
            let
                newMessages =
                    remove message stringList
            in
            { session | message = Just newMessages }

        Nothing ->
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
