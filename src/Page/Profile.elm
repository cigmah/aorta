module Page.Profile exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(..))
import Page.Elements as Elements
import RemoteData exposing (RemoteData(..), WebData)
import Types.Contact as Contact
import Types.Credentials as Credentials exposing (Auth(..), Credentials)
import Types.Login as Login
import Types.Register as Register
import Types.Request as Request
import Types.Session as Session exposing (Session)
import Types.Styles exposing (tailwind)
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
    , responseResponses : WebData ()
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
            , Cmd.none
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
                ( { model | session = Session.addMessage session "You can't have a blank username." }, Cmd.none )

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

                Failure error ->
                    let
                        wrapError errorMessage =
                            ( { model
                                | session = Session.addMessage session errorMessage
                                , screen = Register { screenData | response = responseRegisterWebData }
                              }
                            , Cmd.none
                            )
                    in
                    case error of
                        Http.BadStatus 500 ->
                            -- TODO Change the response code to something that isn't 500...and update both frontend and backend.
                            wrapError "Someone with that username and/or email already exists. Please try another username. If the problem persists, let us know."

                        _ ->
                            wrapError "There was an error with registration. We apologise for the inconvenience. Try again later or get in touch with us."

                _ ->
                    ignore


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
                ( { model | session = Session.addMessage session "You need to fill in both your username and password." }, Cmd.none )

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
                    -- TODO handle network errors etc. separately
                    ( { model
                        | session = Session.addMessage session "That login didn't work. If you think it should have, please get in touch with us."
                        , screen = Login { screenData | response = responseLoginWebData }
                      }
                    , Cmd.none
                    )

                _ ->
                    ignore


updateProfile : ProfileSubMsg -> ProfileScreenData -> Model -> ( Model, Cmd Msg )
updateProfile msg screenData ({ session } as model) =
    let
        ignore =
            ( model, Cmd.none )
    in
    case msg of
        ClickedLogout ->
            let
                newSession =
                    { session | auth = Guest }
            in
            ( { model | session = newSession }, Session.save newSession )



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
            [ div [] [] ]


viewLoginScreen : Model -> LoginScreenData -> List (Html Msg)
viewLoginScreen model screenData =
    let
        loading =
            case screenData.response of
                Loading ->
                    True

                _ ->
                    False
    in
    [ Elements.safeCenter
        [ Elements.articleCard
            { header = text "Login"
            , body =
                Html.form [ onSubmit <| LoginMsg LoginClickedSubmit ]
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
            , footer =
                Just <|
                    Elements.button
                        { text = "Login"
                        , type_ = "submit"
                        , onClick = NoOp
                        , loading = loading
                        }
            }
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
    in
    [ Elements.safeCenter
        [ Elements.articleCard
            { header = text "Register"
            , body =
                Html.form [ onSubmit <| RegisterMsg RegisterClickedSubmit ]
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
                            ]
                        , onClick ToggledLoginRegister
                        ]
                        [ text "Already registered? Click here to login."
                        ]
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
            , footer =
                Just <|
                    Elements.button
                        { text = "Register"
                        , type_ = "submit"
                        , onClick = NoOp
                        , loading = loading
                        }
            }
        ]
    ]


viewProfileScreen : Model -> ProfileScreenData -> List (Html Msg)
viewProfileScreen model screenData =
    [ Elements.container
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



-- Cards
{-
    cardIntro : Html Msg
   cardIntro =
   article
   [][ header [] [ h1 [] [ text "About" ] ]
   , section [ class "markdown" ]
   [ p []
   [ strong [] [ text "AORTA " ]
   , text " is "
   , strong [] [ text "an open revision tool for assessments" ]
   , text ". It is a project of "
   , a [ href "https://cigmah.github.io/" ] [ text "CIGMAH" ]
   , text ", the Coding Interest Group in Medicine and Healthcare. We are medical students with an interest in computer programming and are based at Monash University."
   ]
   , p []
   [ text "This tool is a free and open source project under the "
   , a [ rel "license", href "https://www.gnu.org/licenses/gpl-3.0.en.html" ] [ text "GNU General Public License v3.0" ]
   , text ". Both the "
   , a [ href "https://github.com/cigmah/aorta" ] [ text "frontend" ]
   , text " and "
   , a [ href "https://github.com/cigmah/aorticroot" ] [ text "backend" ]
   , text " code are available from our "
   , a [ href "https://github.com/cigmah" ] [ text " GitHub organisation" ]
   , text ". We welcome pull requests. "
   ]
   , p []
   [ text "Content on this website is written by users and volunteers"
   ]
   ]
   ]

   tailwindButton =
   tailwind
   [ "border-2"
   , "bg-white"
   , "hover:bg-blue-500"
   , "hover:text-white"
   , "border-blue-500"
   , "text-sm"
   , "uppercase"
   , "font-bold"
   , "mx-2"
   ]

   tailwindLabel : Attribute msg
   tailwindLabel =
   tailwind
   [ "uppercase"
   , "text-xs"
   , "font-bold"
   , "text-gray-700"
   ]

   cardVersion : Html Msg
   cardVersion =
   article [][ header [] [ h1 [] [ text "Version" ] ]
   , section []
   [ p []
   [ text "This tool is currently on "
   , strong [] [ text "version 0.1" ]
   , text "."
   ]
   ]
   ]

   cardUser : Model -> Html Msg
   cardUser model =
   case model.session.auth of
   Guest ->
   article [][ header [] [ h1 [] [ text "User" ] ]
   , section [] [ p [] [ text "You are not logged in." ] ]
   , footer []
   [ button [ tailwindButton, onClick ClickedOpenRegisterscreen ] [ text "Register" ]
   , button [ tailwindButton, onClick ClickedOpenLoginscreen ] [ text "Login" ]
   ]
   ]

           User credentials ->
               article []
                   [ header [] [ h1 [] [ text "User" ] ]
                   , section []
                       [ p []
                           [ text "You are logged in as "
                           , strong [] [ text credentials.username ]
                           , text "."
                           ]
                       ]
                   , footer []
                       [ button [ tailwindButton, onClick ClickedLogout ] [ text "Logout" ] ]
                   ]

   -- screens
   -- Register screen

   viewRegisterData : Register.Data -> Html RegisterSubMsg
   viewRegisterData data =
   section [ class "markdown" ]
   [ section [ class "explanation" ]
   [ p [] [ text "To register, we only require you to provide a username. \\n This username may be shown publicly on leaderboards or content you contribute, so we ask you to keep it appropriate and recommend you do not use your email as your username." ]
   , p [] [ text "When you click register, you will be given a randomly-generated password on screen for future use.\\n It is your responsibility to remember or safely store this password.\\n This password is hashed in our database.\\n For further security, we do not let users choose their own passwords." ]
   , p [][ text "Providing an email address is optional; we give you this choice so you can provide as little information as you would like. Providing an email address allows us to generate a new random password for you to use if you forget or lose yours. If you do not provide an email address and lose your password, you will have to make a new account. We do not share or use your email for any other purpose." ]
   ]
   , section [ class "controls", tailwind [ "mt-4" ] ][ div [ class "field" ]
   [ label [ tailwindLabel, for "register-username" ] [ text "Username" ]
   , input
   [ type_ "text"
   , name "register-username"
   , id "register-username"
   , placeholder "Username"
   , required True
   , value data.username
   , onInput RegisterChangedUsername
   ]
   []
   ]
   , div [ class "field" ]
   [ label [ tailwindLabel, for "register-email" ] [ text "Email" ]
   , input
   [ type_ "email"
   , name "register-email"
   , id "register-email"
   , placeholder "Email"
   , required False
   , value data.email
   , onInput RegisterChangedEmail
   ]
   []
   ]
   ]
   ]

   viewRegisterResponse : Register.Response -> Html Msg
   viewRegisterResponse data =
   section [][ p []
   [ text "Thank you for registering, "
   , strong [] [ text data.username ]
   , text ". Your randomly generated password is "
   , strong [] [ text data.password ]
   , text ". Please keep this password safe for future logins."
   ]
   ]

   hideRegisterSubmit : Model -> Bool
   hideRegisterSubmit model =
   case model.screen of
   Register \_ ->
   False

           _ ->
               True

   screenRegister : Model -> Html Msg
   screenRegister model =
   let
   body =
   case model.screen of
   Register data ->
   viewRegisterData data |> Html.map RegisterMsg

                   RegisterResponse data ->
                       viewRegisterResponse data

                   _ ->
                       div [] []

           submitButtonContent =
               case model.screen of
                   Register data ->
                       if data.loading then
                           text "Loading"

                       else
                           text "Submit"

                   _ ->
                       div [] []
       in
       section
           [ class "screen"
           , classList [ ( "hidden", hideRegister model ) ]
           ]
           [ article []
               [ header []
                   [ h1 [] [ text "Register" ]
                   , button [ onClick ClickedClosescreen ]
                       [ i [ class "material-icons" ] [ text "close" ] ]
                   ]
               , body
               , footer []
                   [ button
                       [ type_ "submit"
                       , classList [ ( "hidden", hideRegisterSubmit model ) ]
                       , tailwindButton
                       , onClick (RegisterMsg RegisterClickedSubmit)
                       ]
                       [ submitButtonContent ]
                   ]
               ]
           ]

   -- Login screen

   viewLoginData : Login.Data -> Html LoginSubMsg
   viewLoginData data =
   section []
   [ section [ class "explanation" ]
   [ p [][ text "To login, please enter your username and the random password you were given when you registered." ]
   , p [] [ text "If you have forgotten your password and provided an email when you registered, please contact us through\\n the contact form with your username and we will generate and email a new password to you." ]
   , p [][ text "If you have forgotten your password and did not provide an email when you registered, you will have to make a new account." ]
   ]
   , section [ class "controls", tailwind [ "mt-4" ] ][ div [ class "field" ]
   [ label [ tailwindLabel, for "login-username" ] [ text "Username" ]
   , input
   [ type_ "text"
   , name "login-username"
   , id "login-username"
   , placeholder "Username"
   , required True
   , onInput LoginChangedUsername
   , value data.username
   ]
   []
   ]
   , div [ class "field" ]
   [ label [ tailwindLabel, for "login-password" ] [ text "Password" ]
   , input
   [ type_ "password"
   , name "login-password"
   , id "login-password"
   , placeholder "Password"
   , required True
   , value data.password
   , onInput LoginChangedPassword
   ]
   []
   ]
   ]
   ]

   viewLoginResponse : Credentials -> Html Msg
   viewLoginResponse data =
   section [][ p []
   [ text "Thank you for logging in. You can now close this screen." ]
   ]

   hideLoginSubmit : Model -> Bool
   hideLoginSubmit model =
   case model.screen of
   Login \_ ->
   False

           _ ->
               True

   screenLogin : Model -> Html Msg
   screenLogin model =
   let
   body =
   case model.screen of
   Login data ->
   viewLoginData data |> Html.map LoginMsg

                   LoginResponse data ->
                       viewLoginResponse data

                   _ ->
                       div [] []

           submitButtonContent =
               case model.screen of
                   Login data ->
                       if data.loading then
                           text "Loading"

                       else
                           text "Submit"

                   _ ->
                       div [] []
       in
       section
           [ class "screen"
           , classList [ ( "hidden", hideLogin model ) ]
           ]
           [ article []
               [ header []
                   [ h1 [] [ text "Login" ]
                   , button [ onClick ClickedClosescreen ]
                       [ i [ class "material-icons" ] [ text "close" ] ]
                   ]
               , body
               , footer []
                   [ button
                       [ type_ "submit"
                       , classList [ ( "hidden", hideLoginSubmit model ) ]
                       , tailwindButton
                       , onClick (LoginMsg LoginClickedSubmit)
                       ]
                       [ submitButtonContent ]
                   ]
               ]
           ]

-}
