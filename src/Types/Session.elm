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
import Types.Test as Test exposing (Test)


type alias Session =
    { message : Maybe (List String)
    , auth : Auth
    , key : Key
    , test : Maybe Test
    }


fillKey : Auth -> Key -> Session
fillKey auth key =
    { message = Nothing
    , auth = auth
    , key = key
    , test = Nothing
    }


default : Key -> Session
default key =
    { message = Nothing, auth = Guest, key = key, test = Nothing }


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


encode : Session -> Value
encode session =
    Encode.object
        [ ( "auth", Credentials.encode session.auth ) ]


decoder : Decoder (Key -> Session)
decoder =
    Decode.map fillKey
        (Decode.field "auth" Credentials.decoder)



-- Ports


port cache : Value -> Cmd msg


save : Session -> Cmd msg
save session =
    session
        |> encode
        |> cache
