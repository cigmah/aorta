module Page.Profile exposing (Model, Msg, eject, init, subscriptions, update, view)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types.Session exposing (Session)
import Version exposing (version)



-- types


type alias ContactData =
    { name : String
    , email : String
    , subject : String
    , body : String
    }


initContactData : ContactData
initContactData =
    { name = "", email = "", subject = "", body = "" }



-- Model


type alias Model =
    { session : Session
    , contactData : ContactData
    }



-- Msg


type Msg
    = NoOp
    | ContactMsg ContactSubMsg


type ContactSubMsg
    = ContactChangedName String
    | ContactChangedEmail String
    | ContactChangedSubject String
    | ContactChangedBody String
    | ContactClickedSubmit
    | ContactGotSubmissionResponse


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , contactData = initContactData
      }
    , Cmd.none
    )


eject : Model -> Session
eject model =
    model.session


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ContactMsg contactSubMsg ->
            updateContact contactSubMsg model


updateContact : ContactSubMsg -> Model -> ( Model, Cmd Msg )
updateContact msg ({ contactData } as model) =
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
            ( model, Cmd.none )

        ContactGotSubmissionResponse ->
            ( model, Cmd.none )



-- View


view : Model -> Document Msg
view model =
    { title = ""
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    [ main_ []
        [ section [ class "screen profile" ]
            [ section []
                [ cardIntro
                , cardVersion
                ]
            , section []
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
    article []
        [ header [] [ h1 [] [ text "About" ] ]
        , section []
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
                , a [ href "https://github.com/cigmah" ] [ text " GitHub organisation " ]
                , text " . We welcome pull requests. "
                ]
            , p []
                [ text "Content on this website is written by users and volunteers, and provided under a "
                , a [ rel "license", href "http://creativecommons.org/licenses/by-sa/4.0/" ]
                    [ text "Creative Commons Attribution-ShareAlike 4.0 International License" ]
                , text "."
                ]
            ]
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
    article []
        [ header [] [ h1 [] [ text "User" ] ]
        , section [] [ p [] [ text "You are not logged in." ] ]
        , footer []
            [ button [] [ text "Register" ]
            , button [] [ text "Login" ]
            ]
        ]


cardContact : Model -> Html ContactSubMsg
cardContact model =
    Html.form []
        [ article []
            [ header [] [ h1 [] [ text "Contact Us" ] ]
            , section []
                [ section [ class "explanation" ]
                    [ p [] [ text "If you have any questions, feedback or feature requests, please get in touch with us." ]
                    , p [] [ text "You can contact us through the form below. A subject and body are required; a name and contact email are optional." ]
                    ]
                , section [ class "controls" ]
                    [ div [ class "field" ]
                        [ label [ for "contact-name" ] [ text "Name" ]
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
                        [ label [ for "contact-email" ] [ text "Email" ]
                        , input
                            [ type_ "text"
                            , name "contact-email"
                            , id "contact-email"
                            , placeholder "Email"
                            , value model.contactData.email
                            , onInput ContactChangedEmail
                            ]
                            []
                        ]
                    , div [ class "field" ]
                        [ label [ for "contact-subject" ] [ text "Subject" ]
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
                        [ label [ for "contact-body" ] [ text "Body" ]
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
                [ input [ type_ "submit" ] []
                ]
            ]
        ]



-- Modals


modalRegister : Model -> Html Msg
modalRegister model =
    section [ class "modal hidden" ]
        [ Html.form []
            [ article []
                [ header [] [ h1 [] [ text "Register" ] ]
                , section []
                    [ section [ class "explanation" ]
                        [ p [] [ text "To register, we only require you to provide a username. \n              This username may be shown publicly on leaderboards or content you contribute, so we ask you to keep it appropriate and recommend you do not use your email as your username." ]
                        , p [] [ text "When you click register, you will be given a randomly-generated password on screen for future use.\n              It is your responsibility to remember or safely store this password.\n              This password is hashed in our database.\n              For further security, we do not let users choose their own passwords." ]
                        , p [] [ text "Providing an email address is optional; we give you this choice so you can provide as little information as you would like. Providing an email address allows us to generate a new random password for you to use if you forget or lose yours. If you do not provide an email address and lose your password, you will have to make a new account. We do not share or use your email for any other purpose." ]
                        ]
                    , section [ class "controls" ]
                        [ div [ class "field" ]
                            [ label [ for "register-username" ] [ text "Username" ]
                            , input
                                [ type_ "text"
                                , name "register-username"
                                , id "register-username"
                                , placeholder "Username"
                                , required True
                                ]
                                []
                            ]
                        , div [ class "field" ]
                            [ label [ for "register-email" ] [ text "Email" ]
                            , input
                                [ type_ "email"
                                , name "register-email"
                                , id "register-email"
                                , placeholder "Email"
                                , required False
                                ]
                                []
                            ]
                        ]
                    ]
                , footer []
                    [ input [ type_ "submit" ]
                        []
                    ]
                ]
            ]
        ]


modalLogin : Model -> Html Msg
modalLogin model =
    section [ class "modal hidden" ]
        [ Html.form []
            [ article []
                [ header [] [ p [] [ text "Login" ] ]
                , section []
                    [ section [ class "explanation" ]
                        [ p [] [ text "To login, please enter your username and the random password you were given when you registered." ]
                        , p [] [ text "If you have forgotten your password and provided an email when you registered, please contact us through\n              the contact form with your username and we will generate and email a new password to you." ]
                        , p [] [ text "If you have forgotten your password and did not provide an email when you registered, you will have to make a new account." ]
                        ]
                    , section [ class "controls" ]
                        [ div [ class "field" ]
                            [ label [ for "login-username" ] [ text "Username" ]
                            , input
                                [ type_ "text", name "login-username", id "login-username", placeholder "Username", required True ]
                                []
                            ]
                        , div [ class "field" ]
                            [ label [ for "login-password" ] [ text "Password" ]
                            , input
                                [ type_ "password"
                                , name "login-password"
                                , id "login-password"
                                , placeholder "Password"
                                , required True
                                ]
                                []
                            ]
                        ]
                    ]
                , footer []
                    [ input [ type_ "submit" ] []
                    ]
                ]
            ]
        ]
