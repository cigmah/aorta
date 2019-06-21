module Types.Session exposing (Session, addMessage, clearMessages)

import Browser.Navigation exposing (Key)
import Types.Credentials exposing (..)


type alias Session =
    { message : Maybe (List String)
    , auth : Auth
    , key : Key
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
