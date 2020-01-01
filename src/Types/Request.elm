module Types.Request exposing (..)

{-| Contains types and requests that are sent to the backend API.

All requests should be routed through this module.

-}

import Http exposing (Header, header)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import RemoteData exposing (WebData)
import RemoteData.Http exposing (Config)
import Secret exposing (apiBase)
import Types.Credentials as Credentials exposing (..)
import Types.Login as Login
import Types.Register as Register
import Url.Builder as Builder


{-| Converts a token to a Token HTTP authentication header.
-}
authHeader : Token -> Header
authHeader token =
    header "Authorization" ("Token " ++ token)


{-| Default configuration for requests without authentication.
-}
noAuthConfig : Config
noAuthConfig =
    { headers = []
    , timeout = Nothing
    , tracker = Nothing
    , risky = False
    }


{-| Converts a token to a reuest configuration with authentication.
-}
authConfig : Token -> Config
authConfig token =
    { headers = [ authHeader token ]
    , timeout = Nothing
    , tracker = Nothing
    , risky = False
    }


{-| Converts an authentication type to a request configuration.
-}
authToConfig : Auth -> Config
authToConfig auth =
    case auth of
        Guest ->
            noAuthConfig

        User credentials ->
            authConfig credentials.token


{-| Builds a URL from a path and a list of query parameters.
-}
buildUrl : List String -> List Builder.QueryParameter -> String
buildUrl stringList queryList =
    case queryList of
        [] ->
            Builder.crossOrigin apiBase stringList queryList
                |> (\url -> String.append url "/")

        _ ->
            Builder.crossOrigin apiBase stringList queryList


{-| A type representing data required to create a POST request.
-}
type alias PostRequest response msg =
    { endpoint : Endpoint
    , body : Decode.Value
    , returnDecoder : Decoder response
    , callback : WebData response -> msg
    , auth : Auth
    , queryList : List Builder.QueryParameter
    }


{-| A type representing data required to create a GET request.
-}
type alias GetRequest response msg =
    { auth : Auth
    , endpoint : Endpoint
    , callback : WebData response -> msg
    , returnDecoder : Decoder response
    , queryList : List Builder.QueryParameter
    }


{-| Converts a post request into a Cmd.
-}
post : PostRequest response msg -> Cmd msg
post request =
    RemoteData.Http.postWithConfig
        (authToConfig request.auth)
        (buildUrl (endpointToUrl request.endpoint) request.queryList)
        request.callback
        request.returnDecoder
        request.body


{-| Converts a get request into a command.
-}
get : GetRequest response msg -> Cmd msg
get request =
    RemoteData.Http.getWithConfig
        (authToConfig request.auth)
        (buildUrl (endpointToUrl request.endpoint) request.queryList)
        request.callback
        request.returnDecoder


{-| Convenience aliance for an ID.
-}
type alias Id =
    Int


{-| Enumeration of all possible endpoints
-}
type Endpoint
    = PostRegister
    | PostLogin


{-| Converts an endpoint to a list of paths.
-}
endpointToUrl : Endpoint -> List String
endpointToUrl endpoint =
    case endpoint of
        PostRegister ->
            [ "users" ]

        PostLogin ->
            [ "users", "authenticate" ]


{-| Data required for registering.
-}
type alias PostRegisterData msg =
    { data : Register.PostData
    , auth : Auth
    , callback : WebData Register.Response -> msg
    }


{-| A registration POST request.
-}
postRegister : PostRegisterData msg -> Cmd msg
postRegister request =
    post
        { endpoint = PostRegister
        , body = Register.encode request.data
        , returnDecoder = Register.decoder
        , callback = request.callback
        , auth = request.auth
        , queryList = []
        }


{-| Data required for logging in.
-}
type alias PostLoginData msg =
    { data : Login.PostData
    , auth : Auth
    , callback : WebData Credentials -> msg
    }


{-| A login POST request.
-}
postLogin : PostLoginData msg -> Cmd msg
postLogin request =
    post
        { endpoint = PostLogin
        , body = Login.encode request.data
        , returnDecoder = Login.decoder
        , callback = request.callback
        , auth = request.auth
        , queryList = []
        }
