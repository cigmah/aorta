module Types.Credentials exposing
    ( Auth(..)
    , Credentials
    , Token
    , Username
    , decoder
    , encode
    )

import Json.Decode as Decode exposing (Decoder, Value)
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


fromAuth : Auth -> Maybe Credentials
fromAuth auth =
    case auth of
        Guest ->
            Nothing

        User credentials ->
            Just credentials
