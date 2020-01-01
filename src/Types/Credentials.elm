module Types.Credentials exposing
    ( Auth(..)
    , Credentials
    , Token
    , Username
    , decoder
    , encode
    )

{-| Containst types related to credentials and authentication.

The authentication scheme is Token authentication, where a username and
password are exchanged for an API token.

-}

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode


{-| An authentication token is a string.
-}
type alias Token =
    String


{-| A username is a string.
-}
type alias Username =
    String


{-| Valid credentials consist of a username and a token.
-}
type alias Credentials =
    { token : Token
    , username : Username
    }


{-| The authentication type may be either a Guest, or a credentialed user.
-}
type Auth
    = Guest
    | User Credentials


{-| Converts an authentication into a value.

This is primarily used for serializing the authentication into a storable
object so that it can be saved in the session.

-}
encode : Auth -> Value
encode auth =
    case auth of
        Guest ->
            Encode.null

        User credentials ->
            Encode.object
                [ ( "token", Encode.string credentials.token )
                , ( "username", Encode.string credentials.username )
                ]


{-| Decodes an authentication.

This is primarily used for deserializing a stored authentication object into
a valid authentication type.

-}
decoder : Decoder Auth
decoder =
    Decode.oneOf
        [ Decode.null Guest
        , Decode.map User
            (Decode.map2 Credentials
                (Decode.field "token" Decode.string)
                (Decode.field "username" Decode.string)
            )
        ]


{-| Converts an authentication type into credentials, or Nothing if it is a guest.
-}
fromAuth : Auth -> Maybe Credentials
fromAuth auth =
    case auth of
        Guest ->
            Nothing

        User credentials ->
            Just credentials
