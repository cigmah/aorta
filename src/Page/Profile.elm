module Page.Profile exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(..))
import RemoteData exposing (RemoteData(..), WebData)
import Types.Contact as Contact
import Types.Credentials as Credentials exposing (Auth(..), Credentials)
import Types.Login as Login
import Types.Register as Register
import Types.Request as Request
import Types.Session as Session exposing (Session)
import Types.Styles exposing (tailwind)
import Version exposing (version)



-- TODO Prevent Enter keypress from closing modals when open.
-- TODO Save credentials on receipt of login token.
-- TODO Show modal on logout
-- TODO Put WebData as response in model and create view from that
-- Model


type alias Model =
    { session : Session
    , contactData : Contact.Data
    , modal : Modal
    }


type Modal
    = None
    | Login Login.Data
    | LoginResponse Credentials
    | Register Register.Data
    | RegisterResponse Register.Response



-- Msg


type Msg
    = NoOp
    | ContactMsg ContactSubMsg
    | RegisterMsg RegisterSubMsg
    | LoginMsg LoginSubMsg
    | ClickedOpenLoginModal
    | ClickedOpenRegisterModal
    | ClickedCloseModal
    | ClickedLogout


type ContactSubMsg
    = ContactChangedName String
    | ContactChangedEmail String
    | ContactChangedSubject String
    | ContactChangedBody String
    | ContactClickedSubmit
    | ContactGotSubmissionResponse (WebData Bool)


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


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , contactData = Contact.init
      , modal = None
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

        ContactMsg contactSubMsg ->
            updateContact contactSubMsg model

        RegisterMsg registerSubMsg ->
            case model.modal of
                Register dataRegister ->
                    updateRegister registerSubMsg dataRegister model

                _ ->
                    ( model, Cmd.none )

        LoginMsg loginSubMsg ->
            case model.modal of
                Login dataLogin ->
                    updateLogin loginSubMsg dataLogin model

                _ ->
                    ( model, Cmd.none )

        ClickedOpenLoginModal ->
            ( { model | modal = Login Login.init }, Cmd.none )

        ClickedOpenRegisterModal ->
            ( { model | modal = Register Register.init }, Cmd.none )

        ClickedCloseModal ->
            ( { model | modal = None }, Cmd.none )

        ClickedLogout ->
            let
                newSession =
                    { session | auth = Guest }
            in
            ( { model | session = newSession }, Session.save newSession )


updateContact : ContactSubMsg -> Model -> ( Model, Cmd Msg )
updateContact msg ({ contactData, session } as model) =
    case msg of
        ContactChangedName value ->
            ( { model | contactData = { contactData | name = value } }, Cmd.none )

        ContactChangedEmail value ->
            ( { model | contactData = { contactData | email = value } }, Cmd.none )

        ContactChangedSubject value ->
            ( { model | contactData = { contactData | subject = value } }, Cmd.none )

        ContactChangedBody value ->
            ( { model | contactData = { contactData | body = value } }, Cmd.none )

        ContactClickedSubmit ->
            if contactData.loading then
                ( model, Cmd.none )

            else
                ( { model | contactData = { contactData | loading = True } }
                , Request.post (postContactData model) |> Cmd.map ContactMsg
                )

        ContactGotSubmissionResponse responseWebData ->
            let
                unloaded =
                    { model | contactData = { contactData | loading = False } }

                cleared =
                    { model | contactData = Contact.init }

                addMessage message =
                    Session.addMessage unloaded.session message

                withMessage newModel message =
                    { newModel | session = addMessage message }
            in
            case responseWebData of
                Success _ ->
                    ( withMessage cleared "Your message was received. Thank you. We will attend to it as soon as possible."
                    , Cmd.none
                    )

                Failure error ->
                    ( withMessage unloaded "There was an error with sending your message. We apologise for the inconvenience."
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


updateRegister : RegisterSubMsg -> Register.Data -> Model -> ( Model, Cmd Msg )
updateRegister msg data ({ session } as model) =
    let
        ignore =
            ( model, Cmd.none )
    in
    case msg of
        RegisterChangedUsername username ->
            ( { model | modal = Register { data | username = username } }, Cmd.none )

        RegisterChangedEmail email ->
            ( { model | modal = Register { data | email = email } }, Cmd.none )

        RegisterClickedSubmit ->
            if data.loading then
                ignore

            else if String.trim data.username == "" then
                ( { model | session = Session.addMessage session "You can't have a blank username." }, Cmd.none )

            else
                ( { model | modal = Register { data | loading = True } }
                , Request.post (postRegisterData model data) |> Cmd.map RegisterMsg
                )

        RegisterGotSubmissionResponse responseRegisterWebData ->
            let
                unloaded =
                    { model | modal = Register { data | loading = False } }
            in
            case responseRegisterWebData of
                Success responseData ->
                    let
                        newSession =
                            { session | auth = User { username = responseData.username, token = responseData.token } }
                    in
                    ( { model
                        | modal = RegisterResponse responseData
                        , session = newSession
                      }
                    , Session.save newSession
                    )

                Failure error ->
                    let
                        wrap errorMessage =
                            ( { unloaded | session = Session.addMessage session errorMessage }, Cmd.none )
                    in
                    case error of
                        Http.BadStatus 500 ->
                            -- TODO Change the response code to something that isn't 500...and update both frontend and backend.
                            wrap "Someone with that username and/or email already exists. Please try another username. If the problem persists, let us know."

                        _ ->
                            wrap "There was an error with registration. We apologise for the inconvenience. Try again later or get in touch with us."

                _ ->
                    ignore


updateLogin : LoginSubMsg -> Login.Data -> Model -> ( Model, Cmd Msg )
updateLogin msg data ({ session } as model) =
    let
        ignore =
            ( model, Cmd.none )
    in
    case msg of
        LoginChangedUsername username ->
            ( { model | modal = Login { data | username = username } }, Cmd.none )

        LoginChangedPassword password ->
            ( { model | modal = Login { data | password = password } }, Cmd.none )

        LoginClickedSubmit ->
            if data.loading then
                ignore

            else if String.trim data.username == "" || String.trim data.password == "" then
                ( { model | session = Session.addMessage session "You need to fill in both your username and password." }, Cmd.none )

            else
                ( { model | modal = Login { data | loading = True } }
                , Request.post (postLoginData model data) |> Cmd.map LoginMsg
                )

        LoginGotSubmissionResponse responseLoginWebData ->
            let
                unloaded =
                    { model | modal = Login { data | loading = True } }
            in
            case responseLoginWebData of
                Success responseData ->
                    let
                        newSession =
                            { session | auth = User responseData }
                    in
                    ( { model
                        | modal = LoginResponse responseData
                        , session = newSession
                      }
                    , Session.save newSession
                    )

                Failure error ->
                    -- TODO handle network errors etc. separately
                    ( { unloaded
                        | session = Session.addMessage session "That login didn't work. If you think it should have, please get in touch with us."
                      }
                    , Cmd.none
                    )

                _ ->
                    ignore



-- Requests


postContactData : Model -> Request.PostRequest Bool ContactSubMsg
postContactData model =
    { endpoint = Request.PostContact
    , body = Contact.encode model.contactData
    , returnDecoder = Contact.responseDecoder
    , callback = ContactGotSubmissionResponse
    , auth = model.session.auth
    , queryList = []
    }


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
    [ main_
        [ id "profile"
        , tailwind
            [ "min-h-screen"
            , "pb-16"
            , "md:pt-16"
            , "pb-0"
            ]
        ]
        [ section
            [ tailwind
                [ "container"
                , "mx-auto"
                , "flex"
                , "flex-col-reverse"
                , "md:flex-row"
                ]
            ]
            [ section
                [ tailwind
                    [ "flex"
                    , "flex-col"
                    ]
                ]
                [ cardIntro
                , cardVersion
                ]
            , section
                [ tailwind
                    [ "flex", "flex-col" ]
                ]
                [ cardUser model
                , cardContact model |> Html.map ContactMsg
                ]
            ]
        ]
    , modalRegister model
    , modalLogin model
    ]



-- Cards


cardIntro : Html Msg
cardIntro =
    article
        []
        [ header [] [ h1 [] [ text "About" ] ]
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
    article []
        [ header [] [ h1 [] [ text "Version" ] ]
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
            article []
                [ header [] [ h1 [] [ text "User" ] ]
                , section [] [ p [] [ text "You are not logged in." ] ]
                , footer []
                    [ button [ tailwindButton, onClick ClickedOpenRegisterModal ] [ text "Register" ]
                    , button [ tailwindButton, onClick ClickedOpenLoginModal ] [ text "Login" ]
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


submitContent : Model -> Html msg
submitContent model =
    if model.contactData.loading then
        text "Loading"

    else
        text "Submit"


cardContact : Model -> Html ContactSubMsg
cardContact model =
    Html.form
        [ onSubmit ContactClickedSubmit
        , classList [ ( "hidden", not (None == model.modal) ) ]
        ]
        [ article []
            [ header [] [ h1 [] [ text "Contact Us" ] ]
            , section []
                [ section [ class "explanation" ]
                    [ p [] [ text "If you have any questions, feedback or feature requests, please get in touch with us." ]
                    , p [] [ text "You can contact us through the form below. A subject and body are required; a name and contact email are optional." ]
                    ]
                , section [ class "controls", tailwind [ "mt-4" ] ]
                    [ div [ class "field" ]
                        [ label [ tailwindLabel, for "contact-name" ] [ text "Name" ]
                        , input
                            [ type_ "text"
                            , name "contact-name"
                            , id "contact-name"
                            , placeholder "Name"
                            , value model.contactData.name
                            , onInput ContactChangedName
                            ]
                            []
                        ]
                    , div [ class "field" ]
                        [ label [ tailwindLabel, for "contact-email" ] [ text "Email" ]
                        , input
                            [ type_ "email"
                            , name "contact-email"
                            , id "contact-email"
                            , placeholder "Email"
                            , value model.contactData.email
                            , onInput ContactChangedEmail
                            ]
                            []
                        ]
                    , div [ class "field" ]
                        [ label [ tailwindLabel, for "contact-subject" ] [ text "Subject" ]
                        , input
                            [ type_ "text"
                            , name "contact-subject"
                            , id "contact-subject"
                            , placeholder "Subject"
                            , required True
                            , value model.contactData.subject
                            , onInput ContactChangedSubject
                            ]
                            []
                        ]
                    , div [ class "field" ]
                        [ label [ tailwindLabel, for "contact-body" ] [ text "Body" ]
                        , textarea
                            [ name "contact-body"
                            , id "contact-body"
                            , placeholder "Body"
                            , required True
                            , rows 8
                            , value model.contactData.body
                            , onInput ContactChangedBody
                            ]
                            []
                        ]
                    ]
                ]
            , footer []
                [ button [ tailwindButton, type_ "submit" ] [ submitContent model ]
                ]
            ]
        ]



-- Modals
-- Register Modal


hideRegister : Model -> Bool
hideRegister model =
    case model.modal of
        Register _ ->
            False

        RegisterResponse _ ->
            False

        _ ->
            True


viewRegisterData : Register.Data -> Html RegisterSubMsg
viewRegisterData data =
    section [ class "markdown" ]
        [ section [ class "explanation" ]
            [ p [] [ text "To register, we only require you to provide a username. \n              This username may be shown publicly on leaderboards or content you contribute, so we ask you to keep it appropriate and recommend you do not use your email as your username." ]
            , p [] [ text "When you click register, you will be given a randomly-generated password on screen for future use.\n              It is your responsibility to remember or safely store this password.\n              This password is hashed in our database.\n              For further security, we do not let users choose their own passwords." ]
            , p [] [ text "Providing an email address is optional; we give you this choice so you can provide as little information as you would like. Providing an email address allows us to generate a new random password for you to use if you forget or lose yours. If you do not provide an email address and lose your password, you will have to make a new account. We do not share or use your email for any other purpose." ]
            ]
        , section [ class "controls", tailwind [ "mt-4" ] ]
            [ div [ class "field" ]
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
    section []
        [ p []
            [ text "Thank you for registering, "
            , strong [] [ text data.username ]
            , text ". Your randomly generated password is "
            , strong [] [ text data.password ]
            , text ". Please keep this password safe for future logins."
            ]
        ]


hideRegisterSubmit : Model -> Bool
hideRegisterSubmit model =
    case model.modal of
        Register _ ->
            False

        _ ->
            True


modalRegister : Model -> Html Msg
modalRegister model =
    let
        body =
            case model.modal of
                Register data ->
                    viewRegisterData data |> Html.map RegisterMsg

                RegisterResponse data ->
                    viewRegisterResponse data

                _ ->
                    div [] []

        submitButtonContent =
            case model.modal of
                Register data ->
                    if data.loading then
                        text "Loading"

                    else
                        text "Submit"

                _ ->
                    div [] []
    in
    section
        [ class "modal"
        , classList [ ( "hidden", hideRegister model ) ]
        ]
        [ article []
            [ header []
                [ h1 [] [ text "Register" ]
                , button [ onClick ClickedCloseModal ]
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



-- Login Modal


hideLogin : Model -> Bool
hideLogin model =
    case model.modal of
        Login _ ->
            False

        LoginResponse _ ->
            False

        _ ->
            True


viewLoginData : Login.Data -> Html LoginSubMsg
viewLoginData data =
    section []
        [ section [ class "explanation" ]
            [ p [] [ text "To login, please enter your username and the random password you were given when you registered." ]
            , p [] [ text "If you have forgotten your password and provided an email when you registered, please contact us through\n              the contact form with your username and we will generate and email a new password to you." ]
            , p [] [ text "If you have forgotten your password and did not provide an email when you registered, you will have to make a new account." ]
            ]
        , section [ class "controls", tailwind [ "mt-4" ] ]
            [ div [ class "field" ]
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
    section []
        [ p []
            [ text "Thank you for logging in. You can now close this modal." ]
        ]


hideLoginSubmit : Model -> Bool
hideLoginSubmit model =
    case model.modal of
        Login _ ->
            False

        _ ->
            True


modalLogin : Model -> Html Msg
modalLogin model =
    let
        body =
            case model.modal of
                Login data ->
                    viewLoginData data |> Html.map LoginMsg

                LoginResponse data ->
                    viewLoginResponse data

                _ ->
                    div [] []

        submitButtonContent =
            case model.modal of
                Login data ->
                    if data.loading then
                        text "Loading"

                    else
                        text "Submit"

                _ ->
                    div [] []
    in
    section
        [ class "modal"
        , classList [ ( "hidden", hideLogin model ) ]
        ]
        [ article []
            [ header []
                [ h1 [] [ text "Login" ]
                , button [ onClick ClickedCloseModal ]
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
