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
import Types.YearLevel as YearLevel exposing (YearLevel)


type alias Session =
    { message : Maybe (List String)
    , auth : Auth
    , key : Key
    , yearLevel : YearLevel
    }


fillKey : Auth -> YearLevel -> Key -> Session
fillKey auth yearLevel key =
    { message = Nothing
    , auth = auth
    , key = key
    , yearLevel = yearLevel
    }


default : Key -> Session
default key =
    { message = Nothing
    , auth = Guest
    , key = key
    , yearLevel = YearLevel.Year1
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
        ]


decoder : Decoder (Key -> Session)
decoder =
    Decode.map2 fillKey
        (Decode.field "auth" Credentials.decoder)
        (Decode.field "year_level" YearLevel.decoder)



-- Ports


port cache : Value -> Cmd msg


save : Session -> Cmd msg
save session =
    session
        |> encode
        |> cache
