module Page.Profile exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

import Architecture.Route as Route exposing (Route)
import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(..))
import Json.Decode as Decode
import Page.Elements as Elements
import RemoteData exposing (RemoteData(..), WebData)
import Types.Contact as Contact
import Types.Credentials as Credentials exposing (Auth(..), Credentials)
import Types.Datetime as Datetime
import Types.Domain as Domain exposing (Domain)
import Types.Login as Login
import Types.Question as Question
import Types.Register as Register
import Types.Request as Request
import Types.Session as Session exposing (Session)
import Types.Specialty as Specialty exposing (Specialty)
import Types.Styles exposing (tailwind)
import Types.Topic as Topic exposing (Topic)
import Types.YearLevel as YearLevel exposing (YearLevel)
import Version exposing (version)



-- TODO Prevent Enter keypress from closing screens when open.
-- TODO Save credentials on receipt of login token.
-- TODO Show screen on logout
-- TODO Put WebData as response in model and create view from that
-- Model


type alias Model =
    { session : Session
    , screen : Screen
    }


type alias LoginScreenData =
    { data : Login.Data, response : WebData Credentials }


type alias RegisterScreenData =
    { data : Register.Data, response : WebData Register.Response }


type alias ProfileScreenData =
    { responseBySpecialty : WebData ()
    , responseByTopic : WebData ()
    , responseResponses : WebData (List Question.ResponseListData)
    , responseQuestions : WebData ()
    }


initialProfile : ProfileScreenData
initialProfile =
    { responseBySpecialty = Loading
    , responseByTopic = Loading
    , responseResponses = Loading
    , responseQuestions = Loading
    }


type Screen
    = Login LoginScreenData
    | Register RegisterScreenData
    | Profile ProfileScreenData
    | Logout



-- Msg


type Msg
    = NoOp
    | ToggledLoginRegister
    | RegisterMsg RegisterSubMsg
    | LoginMsg LoginSubMsg
    | ProfileMsg ProfileSubMsg
    | ConfirmedLogout


type RegisterSubMsg
    = RegisterChangedUsername String
    | RegisterChangedEmail String
    | RegisterClickedSubmit
    | RegisterGotSubmissionResponse (WebData Register.Response)


type LoginSubMsg
    = LoginChangedUsername String
    | LoginChangedPassword String
    | LoginClickedSubmit
    | LoginGotSubmissionResponse (WebData Credentials)


type ProfileSubMsg
    = ClickedLogout
    | ProfileGotResponseHistory (WebData (List Question.ResponseListData))


init : Session -> ( Model, Cmd Msg )
init session =
    case session.auth of
        Guest ->
            ( { session = session
              , screen = Login { data = Login.init, response = NotAsked }
              }
            , Cmd.none
            )

        User username ->
            ( { session = session
              , screen = Profile initialProfile
              }
            , Request.get (getQuestionResponseHistory session)
            )


eject : Model -> Session
eject model =
    model.session


inject : Model -> Session -> ( Model, Cmd Msg )
inject model session =
    ( { model | session = session }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ session } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ToggledLoginRegister ->
            case model.screen of
                Login _ ->
                    ( { model | screen = Register { data = Register.init, response = NotAsked } }, Cmd.none )

                Register _ ->
                    ( { model | screen = Login { data = Login.init, response = NotAsked } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        RegisterMsg registerSubMsg ->
            case model.screen of
                Register dataRegister ->
                    updateRegister registerSubMsg dataRegister model

                _ ->
                    ( model, Cmd.none )

        LoginMsg loginSubMsg ->
            case model.screen of
                Login dataLogin ->
                    updateLogin loginSubMsg dataLogin model

                _ ->
                    ( model, Cmd.none )

        ProfileMsg profileSubMsg ->
            case model.screen of
                Profile dataProfile ->
                    updateProfile profileSubMsg dataProfile model

                _ ->
                    ( model, Cmd.none )

        ConfirmedLogout ->
            case model.screen of
                Logout ->
                    init session

                _ ->
                    ( model, Cmd.none )


updateRegister : RegisterSubMsg -> RegisterScreenData -> Model -> ( Model, Cmd Msg )
updateRegister msg screenData ({ session } as model) =
    let
        ignore =
            ( model, Cmd.none )

        data =
            screenData.data

        wrap newData =
            { model | screen = Register { screenData | data = newData } }
    in
    case msg of
        RegisterChangedUsername username ->
            ( wrap { data | username = username }, Cmd.none )

        RegisterChangedEmail email ->
            ( wrap { data | email = email }, Cmd.none )

        RegisterClickedSubmit ->
            if data.loading then
                ignore

            else if String.trim data.username == "" then
                ( { model | screen = Register { screenData | response = Failure (Http.BadStatus 400) } }, Cmd.none )

            else if String.contains " " data.username then
                ( { model | screen = Register { screenData | response = Failure (Http.BadStatus 400) } }, Cmd.none )

            else
                ( { model | screen = Register { screenData | response = Loading } }
                , Request.post (postRegisterData model data) |> Cmd.map RegisterMsg
                )

        RegisterGotSubmissionResponse responseRegisterWebData ->
            case responseRegisterWebData of
                Success responseData ->
                    let
                        newSession =
                            { session | auth = User { username = responseData.username, token = responseData.token } }
                    in
                    ( { model
                        | screen = Register { screenData | response = responseRegisterWebData }
                        , session = newSession
                      }
                    , Session.save newSession
                    )

                _ ->
                    ( { model | screen = Register { screenData | response = responseRegisterWebData } }
                    , Cmd.none
                    )


updateLogin : LoginSubMsg -> LoginScreenData -> Model -> ( Model, Cmd Msg )
updateLogin msg screenData ({ session } as model) =
    let
        ignore =
            ( model, Cmd.none )

        data =
            screenData.data

        wrap newData =
            { model | screen = Login { screenData | data = newData } }
    in
    case msg of
        LoginChangedUsername username ->
            ( wrap { data | username = username }, Cmd.none )

        LoginChangedPassword password ->
            ( wrap { data | password = password }, Cmd.none )

        LoginClickedSubmit ->
            if data.loading then
                ignore

            else if String.trim data.username == "" || String.trim data.password == "" then
                ( { model | screen = Login { screenData | response = Failure (Http.BadStatus 400) } }, Cmd.none )
                -- Very rudimentary validation - backend will validate again and reject if not conforming

            else
                ( { model | screen = Login { screenData | response = Loading } }
                , Request.post (postLoginData model data) |> Cmd.map LoginMsg
                )

        LoginGotSubmissionResponse responseLoginWebData ->
            case responseLoginWebData of
                Success responseData ->
                    let
                        newSession =
                            { session | auth = User responseData }
                    in
                    ( { model
                        | screen = Login { screenData | response = responseLoginWebData }
                        , session = newSession
                      }
                    , Session.save newSession
                    )

                Failure error ->
                    ( { model | screen = Login { screenData | response = responseLoginWebData } }
                    , Cmd.none
                    )

                _ ->
                    ignore


updateProfile : ProfileSubMsg -> ProfileScreenData -> Model -> ( Model, Cmd Msg )
updateProfile msg screenData ({ session } as model) =
    let
        ignore =
            ( model, Cmd.none )

        wrap newData =
            ( { model | screen = Profile newData }, Cmd.none )
    in
    case msg of
        ClickedLogout ->
            let
                newSession =
                    { session | auth = Guest }
            in
            ( { model | session = newSession, screen = Logout }, Session.save newSession )

        ProfileGotResponseHistory webData ->
            wrap { screenData | responseResponses = webData }



-- Requests


postLoginData : Model -> Login.Data -> Request.PostRequest Credentials LoginSubMsg
postLoginData model data =
    { endpoint = Request.PostLogin
    , body = Login.encode data
    , returnDecoder = Login.responseDecoder
    , callback = LoginGotSubmissionResponse
    , auth = model.session.auth
    , queryList = []
    }


postRegisterData : Model -> Register.Data -> Request.PostRequest Register.Response RegisterSubMsg
postRegisterData model data =
    { endpoint = Request.PostRegister
    , body = Register.encode data
    , returnDecoder = Register.responseDecoder
    , callback = RegisterGotSubmissionResponse
    , auth = model.session.auth
    , queryList = []
    }


getQuestionResponseHistory : Session -> Request.GetRequest (List Question.ResponseListData) Msg
getQuestionResponseHistory session =
    { endpoint = Request.GetQuestionResponseHistory
    , returnDecoder = Decode.field "results" (Decode.list Question.decoderResponseList)
    , callback = ProfileMsg << ProfileGotResponseHistory
    , auth = session.auth
    , queryList = []
    }



-- View


view : Model -> Document Msg
view model =
    { title = "AORTA - Profile"
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    case model.screen of
        Login screenData ->
            viewLoginScreen model screenData

        Register screenData ->
            viewRegisterScreen model screenData

        Profile screenData ->
            viewProfileScreen model screenData

        Logout ->
            [ Elements.safeCenter
                [ Elements.articleCard
                    { header = text "Logged Out"
                    , body =
                        div [ class "markdown" ]
                            [ p [] [ text "Thank you for using AORTA. You have successfully logged out." ]
                            , p [] [ a [ Route.toHref Route.Home ] [ text "Click here to return home." ] ]
                            ]
                    , footer = Nothing
                    }
                ]
            ]


viewLoginScreen : Model -> LoginScreenData -> List (Html Msg)
viewLoginScreen model screenData =
    let
        loading =
            case screenData.response of
                Loading ->
                    True

                _ ->
                    False

        isSuccessful =
            case screenData.response of
                Success _ ->
                    True

                _ ->
                    False

        loginButtonText =
            if loading then
                "Loading"

            else
                "Login"

        footer =
            if isSuccessful then
                Nothing

            else
                Just <|
                    div [ tailwind [ "flex" ] ]
                        [ maybeMessage
                        , Elements.button
                            { text = loginButtonText
                            , type_ = "submit"
                            , onClick = LoginMsg LoginClickedSubmit
                            , loading = loading
                            }
                        ]

        successfulInfo =
            case screenData.response of
                Success data ->
                    div
                        []
                        [ p []
                            [ text "You have successfully logged in as "
                            , strong []
                                [ text data.username
                                , text "."
                                ]
                            ]
                        , p [ class "markdown", tailwind [ "my-4", "text-center", "font-bold", "px-4" ] ]
                            [ text "Now that you're registered and logged in, head over to the "
                            , a [ Route.toHref Route.Home ] [ text "grid of notes" ]
                            , text " or "
                            , a [ Route.toHref Route.Revise ] [ text "start an EMQ test." ]
                            ]
                        ]

                _ ->
                    div [] []

        maybeMessage =
            case screenData.response of
                Failure e ->
                    let
                        wrap message =
                            div
                                [ tailwind
                                    [ "text-red-600"
                                    , "bg-red-200"
                                    , "rounded"
                                    , "px-2"
                                    , "py-1"
                                    , "text-sm"
                                    , "font-bold"
                                    , "normal-case"
                                    ]
                                ]
                                [ text message ]
                    in
                    case e of
                        Http.BadStatus 500 ->
                            wrap <| "There was an error with status code 500. This shouldn't happen - please let us know!"

                        Http.BadStatus code ->
                            wrap <| "That username and password didn't match our records. If you're having trouble, you can contact us on the Info tab."

                        Http.NetworkError ->
                            wrap "There was a network error. Please try again later."

                        _ ->
                            wrap "There was an error. Please try again later."

                _ ->
                    div [] []
    in
    [ Elements.safeCenter
        [ section [ tailwind [ "md:w-1/2", "xl:w-1/3" ] ]
            [ Elements.articleCard
                { header = text "Login"
                , body =
                    div []
                        [ Html.form
                            [ onSubmit <| LoginMsg LoginClickedSubmit
                            , classList [ ( "hidden", isSuccessful ) ]
                            ]
                            [ div
                                [ tailwind
                                    [ "text-blue-700"
                                    , "cursor-pointer"
                                    , "hover:text-blue-500"
                                    , "text-sm"
                                    , "text-center"
                                    , "mb-4"
                                    , "flex"
                                    , "items-center"
                                    , "justify-center"
                                    ]
                                , onClick ToggledLoginRegister
                                ]
                                [ text "Not registered? Click here to register."
                                ]
                            , Elements.field
                                [ Elements.label "Username" "username"
                                , Elements.textInput
                                    { value = screenData.data.username
                                    , onInput = LoginMsg << LoginChangedUsername
                                    , placeholder = "Username"
                                    , type_ = "text"
                                    , id = "username"
                                    , required = True
                                    }
                                ]
                            , Elements.field
                                [ Elements.label "Password" "password"
                                , Elements.textInput
                                    { value = screenData.data.password
                                    , onInput = LoginMsg << LoginChangedPassword
                                    , placeholder = "Password"
                                    , type_ = "password"
                                    , id = "password"
                                    , required = True
                                    }
                                ]
                            ]
                        , successfulInfo
                        ]
                , footer = footer
                }
            ]
        ]
    ]


viewRegisterScreen : Model -> RegisterScreenData -> List (Html Msg)
viewRegisterScreen model screenData =
    let
        loading =
            case screenData.response of
                Loading ->
                    True

                _ ->
                    False

        registerButtonText =
            if loading then
                "Loading"

            else
                "Register"

        isSuccessful =
            case screenData.response of
                Success _ ->
                    True

                _ ->
                    False

        footer =
            if isSuccessful then
                Nothing

            else
                Just <|
                    div [ tailwind [ "flex" ] ]
                        [ maybeMessage
                        , Elements.button
                            { text = registerButtonText
                            , type_ = "submit"
                            , onClick = RegisterMsg RegisterClickedSubmit
                            , loading = loading
                            }
                        ]

        successfulInfo =
            case screenData.response of
                Success data ->
                    div
                        []
                        [ p [] [ text "Your registration was successful. Here are your details:" ]
                        , div [ tailwind [ "w-auto", "mx-auto", "block", "my-2" ] ]
                            [ div [ tailwind [ "flex", "items-center" ] ]
                                [ div [ tailwind [ "uppercase", "text-gray-700", "text-sm", "font-bold", "mr-4" ] ] [ text "username:" ]
                                , div [ tailwind [ "font-bold" ] ] [ text data.username ]
                                ]
                            , div [ tailwind [ "flex", "items-center" ] ]
                                [ div [ tailwind [ "uppercase", "text-gray-700", "text-sm", "font-bold", "mr-4" ] ] [ text "password:" ]
                                , div [ tailwind [ "font-bold" ] ] [ text data.password ]
                                ]
                            ]
                        , p [] [ text "You are now logged in for the first time. Please store this password for future logins." ]
                        , p [ class "markdown", tailwind [ "my-4", "text-center", "font-bold", "px-4" ] ]
                            [ text "Now that you're registered and logged in, head over to the "
                            , a [ Route.toHref Route.Home ] [ text "grid of notes" ]
                            , text " or "
                            , a [ Route.toHref Route.Revise ] [ text "start an EMQ test." ]
                            ]
                        ]

                _ ->
                    div [] []

        maybeMessage =
            case screenData.response of
                Failure e ->
                    let
                        wrap message =
                            div
                                [ tailwind
                                    [ "text-red-600"
                                    , "bg-red-200"
                                    , "rounded"
                                    , "px-2"
                                    , "py-1"
                                    , "text-sm"
                                    , "font-bold"
                                    , "normal-case"
                                    ]
                                ]
                                [ text message ]
                    in
                    case e of
                        Http.BadStatus 409 ->
                            wrap "That username or email was taken. Try changing your username - or maybe you've registered before?"

                        Http.BadStatus 400 ->
                            wrap "Your username or email was invalid - try using only alphanumeric characters (no spaces) in your username."

                        Http.BadStatus code ->
                            wrap <| "There was an error with status code " ++ String.fromInt code ++ ". Please try again later."

                        Http.NetworkError ->
                            wrap "There was a network error. Please try again later."

                        _ ->
                            wrap "There was an error. Please try again later."

                _ ->
                    div [] []
    in
    [ Elements.safeCenter
        [ section [ tailwind [ "md:w-2/3", "xl:w-1/2" ] ]
            [ Elements.articleCard
                { header = text "Register"
                , body =
                    div []
                        [ Html.form
                            [ onSubmit <|
                                RegisterMsg RegisterClickedSubmit
                            , classList
                                [ ( "hidden", isSuccessful ) ]
                            ]
                            [ div
                                [ tailwind
                                    [ "text-blue-700"
                                    , "cursor-pointer"
                                    , "hover:text-blue-500"
                                    , "text-sm"
                                    , "text-center"
                                    , "mb-4"
                                    , "flex"
                                    , "items-center"
                                    , "justify-center"
                                    ]
                                , onClick ToggledLoginRegister
                                ]
                                [ text "Already registered? Click here to login."
                                ]
                            , p [ tailwind [ "mb-4", "text-gray-800" ] ]
                                [ text "Choose a username - we will generate a random password for you and show it to you on screen. Please store this password for future logins. If you think you may lose your password, you can optionally provide an email address so we can reset it." ]
                            , Elements.field
                                [ Elements.label "Username (required)" "username"
                                , Elements.textInput
                                    { value = screenData.data.username
                                    , onInput = RegisterMsg << RegisterChangedUsername
                                    , placeholder = "Username"
                                    , type_ = "text"
                                    , id = "username"
                                    , required = True
                                    }
                                ]
                            , Elements.field
                                [ Elements.label "Email (optional)" "email"
                                , Elements.textInput
                                    { value = screenData.data.email
                                    , onInput = RegisterMsg << RegisterChangedEmail
                                    , placeholder = "Email"
                                    , type_ = "email"
                                    , id = "email"
                                    , required = False
                                    }
                                ]
                            ]
                        , successfulInfo
                        ]
                , footer = footer
                }
            ]
        ]
    ]


viewProfileScreen : Model -> ProfileScreenData -> List (Html Msg)
viewProfileScreen model screenData =
    let
        responseTable =
            case screenData.responseResponses of
                Success data ->
                    div []
                        [ table
                            [ tailwind
                                [ "text-sm"
                                , "text-gray-700"
                                , "w-full"
                                ]
                            ]
                            (tr [ tailwind [ "bg-gray-200", "text-gray-700" ] ]
                                [ th [ tailwind [ "border-2", "border-white" ] ] [ text "Question ID" ]
                                , th [ tailwind [ "border-2", "border-white" ] ] [ text "Timestamp" ]
                                , th [ tailwind [ "border-2", "border-white" ] ] [ text "Specialty" ]
                                , th [ tailwind [ "border-2", "border-white" ] ] [ text "Topic" ]
                                , th [ tailwind [ "border-2", "border-white" ] ] [ text "Year Level" ]
                                , th [ tailwind [ "border-2", "border-white" ] ] [ text "Domain" ]
                                , th [ tailwind [ "border-2", "border-white" ] ] [ text "Mark" ]
                                ]
                                :: List.map viewQuestionResponse data
                            )
                        ]

                Loading ->
                    div
                        [ tailwind [ "flex", "h-64", "justify-center", "items-center" ]
                        ]
                        [ div [ class "loading" ] [] ]

                Failure e ->
                    Elements.wrapError e

                NotAsked ->
                    div [] []
    in
    [ Elements.safeMain
        [ Elements.container
            [ div [ tailwind [ "flex", "flex-col-reverse", "md:flex-row", "w-full" ] ]
                [ div [ tailwind [ "flex-grow", "w-full" ] ]
                    [ Elements.articleCard
                        { header = text "Response History"
                        , body =
                            responseTable
                        , footer = Nothing
                        }
                    ]
                , div [ tailwind [] ]
                    [ Elements.articleCard
                        { header = text "Logout"
                        , body =
                            Elements.button
                                { text = "Logout"
                                , type_ = "button"
                                , onClick = ProfileMsg ClickedLogout
                                , loading = False
                                }
                        , footer = Nothing
                        }
                    ]
                ]
            ]
        ]
    ]


viewQuestionResponse : Question.ResponseListData -> Html Msg
viewQuestionResponse data =
    let
        toMarkDiv =
            if data.wasCorrect then
                div
                    [ tailwind
                        [ "rounded-full"
                        , "w-5"
                        , "h-5"
                        , "bg-green-200"
                        , "text-green-600"
                        , "font-bold"
                        , "text-xs"
                        , "flex"
                        , "justify-center"
                        , "items-center"
                        ]
                    ]
                    [ i
                        [ class "material-icons"
                        , tailwind [ "text-sm", "font-bold" ]
                        ]
                        [ text "check" ]
                    ]

            else
                div
                    [ tailwind
                        [ "rounded-full"
                        , "w-5"
                        , "h-5"
                        , "bg-red-200"
                        , "text-red-600"
                        , "font-bold"
                        , "text-xs"
                        , "flex"
                        , "justify-center"
                        , "items-center"
                        ]
                    ]
                    [ i
                        [ class "material-icons"
                        , tailwind [ "text-sm", "font-bold" ]
                        ]
                        [ text "clear" ]
                    ]
    in
    tr
        [ tailwind [] ]
        [ td [ tailwind [ "py-1", "px-2", "markdown" ] ] [ a [ Route.toHref (Route.Question data.questionId) ] [ text ("ID " ++ String.fromInt data.questionId) ] ]
        , td [ tailwind [ "px-1" ] ] [ text (data.answeredDatetime |> Datetime.posixToString) ]
        , td [ tailwind [ "px-1" ] ] [ text (data.specialty |> Specialty.toString) ]
        , td [ tailwind [ "px-1" ] ] [ text (data.topic |> Topic.toBrief) ]
        , td [ tailwind [ "px-1" ] ] [ text (data.yearLevel |> YearLevel.toString) ]
        , td [ tailwind [ "px-1" ] ] [ text (data.domain |> Domain.toString) ]
        , td [ tailwind [ "flex", "justify-center", "items-center", "py-2", "px-2" ] ] [ toMarkDiv ]
        ]
