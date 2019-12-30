module Types.User exposing (GetData, decoder)

{-| Contains the PublicUser type and related functions.

The User type is to represent Users on the website, not the logged in user.

-}

import Json.Decode as Decode exposing (Decoder)


{-| A public user, consisting of an ID and a username.
-}
type alias GetData =
    { id : Int
    , username : String
    }


{-| A JSON decoder for a public user. 
-}
decoder : Decoder GetData
decoder =
    Decode.map2 GetData
        (Decode.field "id" Decode.int)
        (Decode.field "username" Decode.string)
