module Architecture.View exposing (view)

{-| Contains the top-level application `view` function.

The `view` function takes an immutable "view" of the model, and outputs HTML
that can emit events of the `Msg` type. All information that needs to be
dynamically rendered _must_ be in the `Model` that is passed to the `view`
function.

-}

import Architecture.Model exposing (Model(..))
import Architecture.Msg exposing (Msg(..))
import Architecture.Parser exposing (isEqual)
import Architecture.Route as Route
import Architecture.Update exposing (eject)
import Browser exposing (Document)
import Dict
import Element.BottomBar as BottomBar
import Element.Empty as Empty
import Element.Form as Form
import Element.MainBody as MainBody
import Element.NavBar as NavBar
import Element.PrimaryButton as PrimaryButton
import Element.PrimaryLink as PrimaryLink
import Element.SmallPopup as SmallPopup
import Element.Text as Text
import Element.TextInput as TextInput
import Element.TinyTextButton as TinyTextButton
import Element.TopNotification as TopNotification
import Html exposing (Html, div)
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Objective as Objective
import Page.ObjectiveList as ObjectiveList
import Page.Profile as Profile
import Page.Question as Question
import Page.Report as Report
import RemoteData exposing (RemoteData(..))
import Types.Credentials exposing (Auth(..), Credentials)
import Types.Icon as Icon
import Types.Register as Register
import Types.Session as Session


{-| The application top-level `view` function.

Each page specifies it's own view as part of it's individual
Model-View-Update architecture. This function simply maps those individual
page views to the application top-level.

-}
view : Model -> Document Msg
view model =
    case model of
        Home subModel ->
            Home.view subModel
                |> viewPage model GotHomeMsg

        NotFound session ->
            NotFound.view session
                |> viewPage model GotNotFoundMsg

        Profile subModel ->
            Profile.view subModel
                |> viewPage model GotProfileMsg

        Objective subModel ->
            Objective.view subModel
                |> viewPage model GotObjectiveMsg

        ObjectiveList subModel ->
            ObjectiveList.view subModel
                |> viewPage model GotObjectiveListMsg

        Question subModel ->
            Question.view subModel
                |> viewPage model GotQuestionMsg

        Report subModel ->
            Report.view subModel
                |> viewPage model GotReportMsg


{-| Wraps a page's title and body.

This function wraps a page's title and body into the top-level. If something
should be applied to every page, such as a navigation bar, then it can be
added into the pipeline here.

Each page's individual view is wrapped in a `<main>` tag.

-}
viewPage : Model -> (subMsg -> Msg) -> Document subMsg -> Document Msg
viewPage model toMsg page =
    let
        title =
            "AORTA - " ++ page.title

        body =
            page.body
                |> List.map (Html.map toMsg)
                |> wrapPage model page
    in
    { title = title
    , body = body
    }


{-| Wraps the page view in a `<main>` tag.
-}
wrapPage : Model -> Document a -> List (Html Msg) -> List (Html Msg)
wrapPage model document children =
    let
        titleProcessed =
            document.title
                |> String.toLower
                |> String.replace " " "-"

        mainId =
            "main-" ++ titleProcessed

        navigationBarMaybe =
            if isEqual (Route.Question 0) model || isEqual Route.Report model then
                Empty.element

            else
                navigationBar model

        bottomBarMaybe =
            if isEqual (Route.Question 0) model || isEqual Route.Report model then
                Empty.element

            else
                bottomBar model
    in
    [ MainBody.element { id = mainId, children = children }
    , navigationBarMaybe
    , bottomBarMaybe
    , serviceWorkerMessage model
    ]


{-| A top notification about whether the service worker has cached new content.
-}
serviceWorkerMessage : Model -> Html Msg
serviceWorkerMessage model =
    let
        session =
            eject model
    in
    case Dict.get "service_worker" session.messages of
        Nothing ->
            Empty.element

        Just message ->
            TopNotification.element
                { text = message
                , onClick = ClickedMessage "service_worker"
                }


{-| A default navigation bar that can wrap pages.
-}
navigationBar : Model -> Html Msg
navigationBar model =
    let
        session =
            eject model

        rightItem =
            case session.auth of
                Guest ->
                    { text = "Login/Register"
                    , onClick = ToggledShowLogin
                    , smallPopup = viewAuthDialog session.authDialog
                    }

                User credentials ->
                    { text = credentials.username
                    , onClick = ToggledShowLogin
                    , smallPopup = viewLogoutDialog credentials session.authDialog
                    }
    in
    NavBar.element
        { regularItems =
            [ { text = "Question Bank"
              , active = isEqual Route.Home model
              , href = Route.toHref Route.Home
              }
            , { text = "Learning Objectives"
              , active = isEqual Route.ObjectiveList model || isEqual (Route.Objective 0) model
              , href = Route.toHref Route.ObjectiveList
              }
            ]
        , rightItem = rightItem
        }


{-| A default bottom bar that can wrap pages.
-}
bottomBar : Model -> Html msg
bottomBar model =
    BottomBar.element
        { children =
            [ { text = "Questions"
              , active = isEqual Route.Home model
              , href = Route.toHref Route.Home
              , icon = Icon.questions
              }
            , { text = "Objectives"
              , active = isEqual Route.ObjectiveList model || isEqual (Route.Objective 0) model
              , href = Route.toHref Route.ObjectiveList
              , icon = Icon.objectives
              }
            ]
        }


viewAuthDialog : Session.AuthDialog -> Maybe (SmallPopup.Data Msg)
viewAuthDialog authDialog =
    case authDialog of
        Session.NoDialog ->
            Nothing

        Session.LoginDialog data webData ->
            Just
                { title = "Login"
                , onClose = ToggledShowLogin
                , body =
                    [ Form.element
                        { onSubmit = ClickedLogin
                        , submitButtonText = "Login"
                        , responseWebData = webData
                        , onSuccessMessage = \_ -> Empty.element
                        , children =
                            [ TextInput.element
                                { value = data.username
                                , information = "The username you provided on registration."
                                , placeholder = "Username"
                                , onInput = ChangedLoginUsername
                                , id = "login-username"
                                , label = "Username"
                                , inputType = "username"
                                , required = True
                                }
                            , TextInput.element
                                { value = data.password
                                , information = "The access code you were provided on registration."
                                , placeholder = "Access Code"
                                , onInput = ChangedLoginPassword
                                , id = "login-password"
                                , label = "Access Code"
                                , inputType = "password"
                                , required = True
                                }
                            ]
                        }
                    , TinyTextButton.element
                        { text = "Not registered? Click here."
                        , onClick = ToggledAuthDialogType
                        , submit = False
                        }
                    ]
                }

        Session.RegisterDialog data webData ->
            Just
                { title = "Register"
                , onClose = ToggledShowLogin
                , body =
                    [ Form.element
                        { onSubmit = ClickedRegister
                        , submitButtonText = "Register"
                        , responseWebData = webData
                        , onSuccessMessage = \_ -> Empty.element
                        , children =
                            [ TextInput.element
                                { value = data.username
                                , information = "The username you'd like to use for future logins."
                                , placeholder = "Username"
                                , onInput = ChangedRegisterUsername
                                , id = "register-username"
                                , label = "New Username"
                                , inputType = "username"
                                , required = True
                                }
                            , TextInput.element
                                { value = data.email
                                , information = "An optional recovery email if you forget your access code."
                                , placeholder = "Email"
                                , onInput = ChangedRegisterEmail
                                , id = "register-email"
                                , label = "Email (optional)"
                                , inputType = "email"
                                , required = False
                                }
                            ]
                        }
                    , TinyTextButton.element
                        { onClick = ToggledAuthDialogType
                        , text = "Already registered? Click here."
                        , submit = False
                        }
                    ]
                }


viewLogoutDialog : Credentials -> Session.AuthDialog -> Maybe (SmallPopup.Data Msg)
viewLogoutDialog credentials authDialog =
    case authDialog of
        Session.NoDialog ->
            Nothing

        Session.LoginDialog data _ ->
            Just
                { title = credentials.username
                , onClose = ToggledShowLogin
                , body =
                    [ Text.smallHeader (String.join "" [ "Hi, ", credentials.username, "!" ])

                    {- removing this until ready...
                       , Text.body "Thanks for using AORTA. Your overall statistics are viewable from the link below:"
                       , PrimaryLink.element
                           { text = "View my Statistics"
                           , href = Route.toHref Route.Profile
                           }
                    -}
                    , Text.body "If you have any questions, let us know at cigmah.contact@gmail.com - we're always happy to respond."
                    , PrimaryButton.element
                        { text = "Logout"
                        , onClick = ClickedLogout
                        }
                    ]
                }

        Session.RegisterDialog data webData ->
            case webData of
                Success responseData ->
                    Just
                        { title = "Thanks for Registering!"
                        , onClose = ToggledShowLogin
                        , body =
                            [ Text.smallHeader (String.join "" [ "Hi ", credentials.username, "," ])
                            , Text.body "Thanks for registering, you've just logged in for the first time!"
                            , Text.body "Here are your credentials for future logins. Please keep this safe somewhere as it won't be shown to you again."
                            , Text.credentialShow { username = responseData.username, password = responseData.password }
                            , PrimaryButton.element
                                { text = "Got it!"
                                , onClick = ToggledShowLogin
                                }
                            ]
                        }

                _ ->
                    -- This state is impossible, or should be impossible anyway
                    Just
                        { title = "Hmm..."
                        , onClose = ToggledShowLogin
                        , body =
                            [ Text.body "Well this is embarassing. Something went wrong, and the particular way this went wrong shouldn't happen. Please let us know so we can fix it. Thank you :)" ]
                        }
