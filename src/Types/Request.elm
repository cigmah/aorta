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
import Types.Comment as Comment
import Types.Credentials as Credentials exposing (..)
import Types.Login as Login
import Types.Objective as Objective
import Types.Paginated as Paginated exposing (Paginated)
import Types.Question as Question
import Types.Register as Register
import Types.Specialty as Specialty exposing (Specialty)
import Types.Stage as Stage exposing (Stage)
import Types.Topic as Topic exposing (Topic)
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


{-| A type representing data required to create a PATCH request.
-}
type alias PatchRequest response msg =
    { endpoint : Endpoint
    , body : Decode.Value
    , returnDecoder : Decoder response
    , callback : WebData response -> msg
    , auth : Auth
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


{-| Converts a patch request into a command.
-}
patch : PatchRequest response msg -> Cmd msg
patch request =
    RemoteData.Http.patchWithConfig
        (authToConfig request.auth)
        (buildUrl (endpointToUrl request.endpoint) request.queryList)
        request.callback
        request.returnDecoder
        request.body


{-| Convenience aliance for an ID.
-}
type alias Id =
    Int


{-| Enumeration of all possible endpoints
-}
type Endpoint
    = PostRegister
    | PostLogin
    | PostObjective
    | GetObjectiveList
    | GetObjective Id
    | GetQuestionList
    | PutObjective Id
    | PostQuestion
    | GetQuestionIdList
    | GetQuestion Id
    | PostQuestionResponse
    | PostQuestionRating
    | PostQuestionComment


{-| Converts an endpoint to a list of paths.
-}
endpointToUrl : Endpoint -> List String
endpointToUrl endpoint =
    case endpoint of
        PostRegister ->
            [ "users" ]

        PostLogin ->
            [ "users", "authenticate" ]

        PostObjective ->
            [ "objectives" ]

        GetObjectiveList ->
            [ "objectives" ]

        GetObjective id ->
            [ "objectives", String.fromInt id ]

        PutObjective id ->
            [ "objectives", String.fromInt id ]

        GetQuestionList ->
            [ "questions" ]

        PostQuestion ->
            [ "questions" ]

        GetQuestionIdList ->
            [ "questions", "test" ]

        GetQuestion id ->
            [ "questions", String.fromInt id ]

        PostQuestionResponse ->
            [ "questions", "responses" ]

        PostQuestionRating ->
            [ "questions", "ratings" ]

        PostQuestionComment ->
            [ "questions", "comments" ]


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


{-| Data required to create a new objective.
-}
type alias PostObjectiveData msg =
    { data : Objective.PostData
    , auth : Auth
    , callback : WebData Objective.GetData -> msg
    }


{-| A objective create POST request.
-}
postObjective : PostObjectiveData msg -> Cmd msg
postObjective request =
    post
        { endpoint = PostObjective
        , body = Objective.encode request.data
        , returnDecoder = Objective.decoder
        , callback = request.callback
        , auth = request.auth
        , queryList = []
        }


{-| Data required to get an objective.
-}
type alias GetObjectiveData msg =
    { id : Id
    , auth : Auth
    , callback : WebData Objective.GetData -> msg
    }


{-| A single objective GET request.
-}
getObjective : GetObjectiveData msg -> Cmd msg
getObjective request =
    get
        { auth = request.auth
        , endpoint = GetObjective request.id
        , callback = request.callback
        , returnDecoder = Objective.decoder
        , queryList = []
        }


{-| Data required to get a list of objectives.
-}
type alias GetObjectiveListData msg =
    { auth : Auth
    , specialtyFilters : List Int
    , topicFilters : List Int
    , stageFilters : List Int
    , search : String
    , page : Int
    , callback : WebData (Paginated Objective.GetData) -> msg
    }


{-| Convers list of ints to specialty query string.
-}
toSpecialtyQueries : List Int -> List Builder.QueryParameter
toSpecialtyQueries list =
    if List.length list == Specialty.enumerable.count then
        []

    else
        list
            |> List.map (Builder.int "specialty")


{-| Converts list of ints to topic query string.
-}
toTopicQueries : List Int -> List Builder.QueryParameter
toTopicQueries list =
    if List.length list == Topic.enumerable.count then
        []

    else
        list
            |> List.map (Builder.int "topic")


{-| Converts list of ints to stage query string.
-}
toStageQueries : List Int -> List Builder.QueryParameter
toStageQueries list =
    if List.length list == Stage.enumerable.count then
        []

    else
        list
            |> List.map (Builder.int "stage")


{-| A GET request for a list of objectives
-}
getObjectiveList : GetObjectiveListData msg -> Cmd msg
getObjectiveList request =
    let
        searchQuery =
            if request.search == "" then
                []

            else
                [ Builder.string "search" request.search ]
    in
    get
        { auth = request.auth
        , endpoint = GetObjectiveList
        , callback = request.callback
        , returnDecoder = Paginated.decoder Objective.decoder
        , queryList =
            List.concat
                [ toSpecialtyQueries request.specialtyFilters
                , toTopicQueries request.topicFilters
                , toStageQueries request.stageFilters
                , searchQuery
                , [ Builder.int "page" request.page ]
                ]
        }


{-| A GET request for a list of basic question data, by objective ID.
-}
type alias GetQuestionBasicListData msg =
    { objectiveId : Id
    , page : Int
    , auth : Auth
    , callback : WebData (Paginated Question.GetBasicData) -> msg
    }


{-| A GET request for a list of questions, by objective ID.
-}
getQuestionBasicList : GetQuestionBasicListData msg -> Cmd msg
getQuestionBasicList request =
    get
        { auth = request.auth
        , endpoint = GetQuestionList
        , callback = request.callback
        , returnDecoder = Paginated.decoder Question.decoderBasic
        , queryList =
            [ Builder.int "page" request.page
            , Builder.int "objective_id" request.objectiveId
            ]
        }


{-| Data required to edit an objective.
-}
type alias PatchObjectiveData msg =
    { data : Objective.EditableData
    , objectiveId : Int
    , auth : Auth
    , callback : WebData Objective.GetData -> msg
    }


{-| A objective edit PATCH request.
-}
patchObjective : PatchObjectiveData msg -> Cmd msg
patchObjective request =
    patch
        { endpoint = PutObjective request.objectiveId
        , body = Objective.encodeEditable request.data
        , returnDecoder = Objective.decoder
        , callback = request.callback
        , auth = request.auth
        , queryList = []
        }


{-| Data required to add a question.
-}
type alias PostQuestionData msg =
    { data : Question.PostData
    , auth : Auth
    , callback : WebData Question.GetBasicData -> msg
    }


{-| A POST request to add a question.
-}
postQuestion : PostQuestionData msg -> Cmd msg
postQuestion request =
    post
        { endpoint = PostQuestion
        , body = Question.encode request.data
        , returnDecoder = Question.decoderBasic
        , callback = request.callback
        , auth = request.auth
        , queryList = []
        }


type alias GetQuestionDetailData msg =
    { questionId : Id
    , auth : Auth
    , callback : WebData Question.GetDetailData -> msg
    }


getQuestionDetail : GetQuestionDetailData msg -> Cmd msg
getQuestionDetail request =
    get
        { auth = request.auth
        , endpoint = GetQuestion request.questionId
        , callback = request.callback
        , returnDecoder = Question.decoderDetail
        , queryList = []
        }


type alias GetQuestionIdListData msg =
    { auth : Auth
    , specialtyFilters : List Int
    , topicFilters : List Int
    , stageFilters : List Int
    , callback : WebData (List Int) -> msg
    }


getQuestionIdList : GetQuestionIdListData msg -> Cmd msg
getQuestionIdList request =
    get
        { auth = request.auth
        , endpoint = GetQuestionIdList
        , callback = request.callback
        , returnDecoder = Decode.list Decode.int
        , queryList =
            List.concat
                [ toSpecialtyQueries request.specialtyFilters
                , toTopicQueries request.topicFilters
                , toStageQueries request.stageFilters
                , [ Builder.string "random" "true" ]
                ]
        }


type alias PostQuestionResponseData msg =
    { auth : Auth
    , callback : WebData () -> msg
    , choiceId : Int
    }


postQuestionResponse : PostQuestionResponseData msg -> Cmd msg
postQuestionResponse request =
    post
        { endpoint = PostQuestionResponse
        , body =
            Encode.object
                [ ( "choice", Encode.int request.choiceId ) ]
        , returnDecoder = Decode.succeed ()
        , callback = request.callback
        , auth = request.auth
        , queryList = []
        }


type alias PostQuestionRatingData msg =
    { auth : Auth
    , callback : WebData () -> msg
    , rating : Int
    , questionId : Int
    }


postQuestionRating : PostQuestionRatingData msg -> Cmd msg
postQuestionRating request =
    post
        { endpoint = PostQuestionRating
        , body =
            Encode.object
                [ ( "question", Encode.int request.questionId )
                , ( "rating", Encode.int request.rating )
                ]
        , returnDecoder = Decode.succeed ()
        , callback = request.callback
        , auth = request.auth
        , queryList = []
        }


type alias PostQuestionCommentData msg =
    { auth : Auth
    , callback : WebData Comment.GetData -> msg
    , comment : String
    , questionId : Int
    }


postQuestionComment : PostQuestionCommentData msg -> Cmd msg
postQuestionComment request =
    post
        { endpoint = PostQuestionComment
        , body =
            Encode.object
                [ ( "question", Encode.int request.questionId )
                , ( "content", Encode.string request.comment )
                ]
        , returnDecoder = Comment.decoder
        , callback = request.callback
        , auth = request.auth
        , queryList = []
        }
