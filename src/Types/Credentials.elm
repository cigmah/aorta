module Types.Credentials exposing (Auth(..), Code, Credentials, Email, Token, Username, encodeEmail)

import Json.Encode as Encode


type alias Token =
    String


type alias Code =
    String


type alias Email =
    String


type alias Username =
    String


type alias Credentials =
    { token : Token
    , username : Username
    , email : Email
    }



-- Authentication


type Auth
    = Guest
    | User Credentials


fromAuth : Auth -> Maybe Credentials
fromAuth auth =
    case auth of
        Guest ->
            Nothing

        User credentials ->
            Just credentials



-- JSON


encodeEmail : Email -> Encode.Value
encodeEmail email =
    Encode.object [ ( "email", Encode.string email ) ]



{-
   This is represented in JSON as a "token", but I've named the variable `code`
   to distinguish it from the token that's persisted and used for subsequent
   authentication
-}


encodeCode : Code -> Encode.Value
encodeCode code =
    Encode.object [ ( "token", Encode.string code ) ]
