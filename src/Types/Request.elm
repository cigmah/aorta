module Types.Request exposing
    ( Endpoint(..)
    , GetRequest
    , PostRequest
    , get
    , post
    )

import Http exposing (Header, header)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import RemoteData exposing (WebData)
import RemoteData.Http exposing (Config)
import Secret exposing (apiBase)
import Types.Credentials as Credentials exposing (..)
import Url.Builder as Builder


authHeader : Token -> Header
authHeader token =
    header "Authorization" ("Token " ++ token)


noAuthConfig : Config
noAuthConfig =
    { headers = []
    , timeout = Nothing
    , tracker = Nothing
    , risky = False
    }


authConfig : Token -> Config
authConfig token =
    { headers = [ authHeader token ]
    , timeout = Nothing
    , tracker = Nothing
    , risky = False
    }


authToConfig : Auth -> Config
authToConfig auth =
    case auth of
        Guest ->
            noAuthConfig

        User credentials ->
            authConfig credentials.token



-- Helpers


buildUrl : List String -> String
buildUrl stringList =
    Builder.crossOrigin apiBase stringList []
        |> (\x -> String.append x "/")


type alias PostRequest response msg =
    { endpoint : Endpoint
    , body : Decode.Value
    , returnDecoder : Decoder response
    , callback : WebData response -> msg
    , auth : Auth
    }


type alias GetRequest response msg =
    { auth : Auth
    , endpoint : Endpoint
    , callback : WebData response -> msg
    , returnDecoder : Decoder response
    }


post : PostRequest response msg -> Cmd msg
post request =
    RemoteData.Http.postWithConfig
        (authToConfig request.auth)
        (buildUrl <| endpointToUrl request.endpoint)
        request.callback
        request.returnDecoder
        request.body


get : GetRequest response msg -> Cmd msg
get request =
    RemoteData.Http.getWithConfig
        (authToConfig request.auth)
        (buildUrl <| endpointToUrl request.endpoint)
        request.callback
        request.returnDecoder



-- Requests


type alias Id =
    Int


type Endpoint
    = GetQuestionList
    | GetQuestion Id
    | GetQuestionRandom
    | GetNoteList
    | GetNote Id
    | PostRegister
    | PostLogin
    | PostResponse
    | PostComment
    | PostContact
    | PostQuestion
    | PostNote


endpointToUrl : Endpoint -> List String
endpointToUrl endpoint =
    case endpoint of
        GetQuestionList ->
            [ "questions" ]

        GetQuestion id ->
            [ "questions", String.fromInt id ]

        GetQuestionRandom ->
            [ "questions", "random" ]

        PostRegister ->
            [ "users" ]

        PostLogin ->
            [ "users", "authenticate" ]

        PostResponse ->
            [ "questions", "responses" ]

        PostComment ->
            [ "notes", "comments" ]

        PostQuestion ->
            [ "questions" ]

        PostContact ->
            [ "mail" ]

        PostNote ->
            [ "notes" ]

        GetNoteList ->
            [ "notes" ]

        GetNote noteId ->
            [ "notes", String.fromInt noteId ]
