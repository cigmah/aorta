port module Types.Session exposing (..)

import Browser.Navigation exposing (Key)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import List.Extra exposing (remove)
import RemoteData exposing (RemoteData(..), WebData)
import Types.Credentials as Credentials exposing (..)
import Types.Request as Request
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
    { message = Nothing
    , auth = Guest
    , key = key
    , test = Nothing
    }



-- Authentication


isGuest : Session -> Bool
isGuest session =
    case session.auth of
        Guest ->
            True

        User _ ->
            False



-- Messages


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



-- Encoding/Decoding


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
        ]


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
