module Types.Credentials exposing (Auth(..), Credentials, Token, Username)

import Json.Encode as Encode


type alias Token =
    String


type alias Username =
    String


type alias Credentials =
    { token : Token
    , username : Username
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
