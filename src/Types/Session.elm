module Types.Session exposing (Session, addMessage)

import Types.Credentials exposing (..)


type alias Session =
    { message : Maybe (List String)
    , auth : Auth
    }


addMessage : Session -> String -> Session
addMessage session message =
    case session.message of
        Just otherMessages ->
            { session | message = Just (message :: otherMessages) }

        Nothing ->
            { session | message = Just [ message ] }
