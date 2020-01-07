module Architecture.Update exposing (eject, update)

{-| Contains the base `update` function for the application.

The `update` function takes a message and a model, and returns the new model
as well as any side effects that should be performed by the Elm runtime. All
reactions to events must come through the `update` function.

-}

import Architecture.Init as Init exposing (extractWith)
import Architecture.Model exposing (Model(..))
import Architecture.Msg exposing (Msg(..))
import Architecture.Parser as Parser
import Architecture.Route as Route exposing (Route)
import Browser
import Browser.Navigation as Navigation
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Objective as Objective
import Page.ObjectiveList as ObjectiveList
import Page.Profile as Profile
import Page.Question as Question
import Page.Report as Report
import RemoteData exposing (RemoteData(..), WebData)
import Types.Request as Request
import Types.Session as Session exposing (Session)
import Url


{-| The applications top-level `update` function.

There are several top-level `Msg` variants, which are fed to the update
function here to provide updates to the whole model, often by manipulating
the shared `Session` object.

All pages have their own individual Model-View-Update architecture, and this
`update` function performs each page's update and subsequently extracts the
result to the top-level application.

-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        -- If the route is changed, then reroute the model and push the route's string to the URL.
        -- TODO: Maybe this can be changed to the same as the UrlChanged message, rerouting may not be necessary
        ( RouteChanged route, _ ) ->
            reroute route model
                |> addCmdMsg
                    (Navigation.pushUrl
                        (.key (eject model))
                        (Route.toString route)
                    )

        -- If a URL is requested, then navigate to the URL.
        ( UrlRequested urlRequest, _ ) ->
            case urlRequest of
                -- An internal URL should not trigger a full page load; change the URL and UrlChanged will update the page
                Browser.Internal internal ->
                    ( model
                    , Navigation.pushUrl
                        (.key (eject model))
                        (Url.toString internal)
                    )

                -- An external page load should trigger a load of the full external page
                Browser.External external ->
                    ( model, Navigation.load external )

        -- If the URL is changed, then eject the `Session` and initialise a page from the new URL
        ( UrlChanged url, _ ) ->
            eject model
                |> Session.removeAuthDialog
                |> Init.fromRoute (Parser.fromUrl url)

        -- If a message is clicked, then clear that particular message from the messages
        ( ClickedMessage string, _ ) ->
            Session.clearMessage string
                |> updateSession model

        -- If the service worker caches new content, add it to the messages
        ( GotServiceWorkerNotification _, _ ) ->
            Session.addMessage ( "service_worker", "We've updated the app since your last visit - please refresh to load the updates." )
                |> updateSession model

        -- If the user toggles the login box, it should either be shown or not shown
        ( ToggledShowLogin, _ ) ->
            Session.toggleAuthDialog
                |> updateSession model

        -- If the user toggles the auth dialog type, then switch to Login or Register
        ( ToggledAuthDialogType, _ ) ->
            Session.toggleAuthDialogType
                |> updateSession model

        ( ClickedLogout, _ ) ->
            let
                ( newSession, cmd ) =
                    eject model
                        |> Session.logout

                ( newModel, _ ) =
                    inject model (Session.removeAuthDialog newSession)
            in
            ( newModel, cmd )

        -- If the user changes their login username, then update
        ( ChangedLoginUsername string, _ ) ->
            Session.changeLoginUsername string
                |> updateSession model

        -- If the user changes their login password, then update
        ( ChangedLoginPassword string, _ ) ->
            Session.changeLoginPassword string
                |> updateSession model

        -- If the user clicks login, send the request
        ( ClickedLogin, _ ) ->
            let
                ( newSession, cmd ) =
                    eject model
                        |> Session.sendLogin GotLoginResponse

                ( newModel, _ ) =
                    inject model newSession
            in
            ( newModel, cmd )

        -- If a login response is received, save it
        ( GotLoginResponse response, _ ) ->
            let
                ( newSession, cmd ) =
                    eject model
                        |> Session.receiveLogin response

                ( newModel, _ ) =
                    inject model newSession
            in
            ( newModel, cmd )

        -- If the user changes their register username, then update
        ( ChangedRegisterUsername string, _ ) ->
            Session.changeRegisterUsername string
                |> updateSession model

        -- If the user changs their register email, then update
        ( ChangedRegisterEmail string, _ ) ->
            Session.changeRegisterEmail string
                |> updateSession model

        -- If the user clicks register, send the request
        ( ClickedRegister, _ ) ->
            let
                ( newSession, cmd ) =
                    eject model
                        |> Session.sendRegister GotRegisterResponse

                ( newModel, _ ) =
                    inject model newSession
            in
            ( newModel, cmd )

        -- If a register response is received, save it
        ( GotRegisterResponse response, _ ) ->
            let
                ( newSession, cmd ) =
                    eject model
                        |> Session.receiveRegister response

                ( newModel, _ ) =
                    inject model newSession
            in
            ( newModel, cmd )

        ( GotHomeMsg subMsg, Home subModel ) ->
            Home.update subMsg subModel
                |> extractWith Home GotHomeMsg

        ( GotNotFoundMsg subMsg, NotFound subModel ) ->
            NotFound.update subMsg subModel
                |> extractWith NotFound GotNotFoundMsg

        ( GotProfileMsg subMsg, Profile subModel ) ->
            Profile.update subMsg subModel
                |> extractWith Profile GotProfileMsg

        ( GotObjectiveMsg subMsg, Objective subModel ) ->
            Objective.update subMsg subModel
                |> extractWith Objective GotObjectiveMsg

        ( GotObjectiveListMsg subMsg, ObjectiveList subModel ) ->
            ObjectiveList.update subMsg subModel
                |> extractWith ObjectiveList GotObjectiveListMsg

        ( GotQuestionMsg subMsg, Question subModel ) ->
            Question.update subMsg subModel
                |> extractWith Question GotQuestionMsg

        ( GotReportMsg subMsg, Report subModel ) ->
            Report.update subMsg subModel
                |> extractWith Report GotReportMsg

        _ ->
            ( model, Cmd.none )


{-| Takes a `Model` and returns only the `Session` object.

This function "ejects" the shared `Session` object out of each page, so that
it can be fed to a new page initialisation function. This is vital to ensure
that the `Session` persists across pages.

`eject` is specified within each individual page (but is the same for each
page) - this function simply maps it to the top-level application.

-}
eject : Model -> Session
eject page =
    case page of
        Home model ->
            Home.eject model

        NotFound model ->
            NotFound.eject model

        Profile model ->
            Profile.eject model

        Objective model ->
            Objective.eject model

        ObjectiveList model ->
            ObjectiveList.eject model

        Question model ->
            Question.eject model

        Report model ->
            Report.eject model


{-| Injects a provided `Session` into a `Model`.

This function "injects" a session into a `Model`, replacing whatever
`Session` was inside that `Model` originally with the provided `Session`.
This is useful for manipulating the `Session` object by first ejecting,
modifying it, then re-injecting it into the original `Model`.

`inject` is specified with each individual page (but is the same for each
page) - this function simply maps it to the top-level application.

-}
inject : Model -> Session -> ( Model, Cmd Msg )
inject page session =
    case page of
        Home model ->
            session
                |> Home.inject model
                |> extractWith Home GotHomeMsg

        NotFound model ->
            session
                |> NotFound.inject model
                |> extractWith NotFound GotNotFoundMsg

        Profile model ->
            session
                |> Profile.inject model
                |> extractWith Profile GotProfileMsg

        Objective model ->
            session
                |> Objective.inject model
                |> extractWith Objective GotObjectiveMsg

        ObjectiveList model ->
            session
                |> ObjectiveList.inject model
                |> extractWith ObjectiveList GotObjectiveListMsg

        Question model ->
            session
                |> Question.inject model
                |> extractWith Question GotQuestionMsg

        Report model ->
            session
                |> Report.inject model
                |> extractWith Report GotReportMsg


{-| Updates a session with an updater, then returns the model again.
-}
updateSession : Model -> (Session -> Session) -> ( Model, Cmd Msg )
updateSession model updater =
    eject model
        |> updater
        |> inject model


{-| Reroutes the `Model` into a new `Page`, returning a new `Model`.

This is a convenience function which navigates the `Model` into a new
provided route, by ejecting the `Session` from the original page and piping
it through to the initialisation function for the new page.

-}
reroute : Route -> Model -> ( Model, Cmd Msg )
reroute route model =
    eject model
        |> Init.fromRoute route


{-| Adds an extra `Cmd Msg` a tupled `Cmd Msg`.

This is a convenience function for adding an extra `Cmd Msg` to a `Cmd Msg`
within a tuple (such as the `(Model, Cmd Msg)` tuple in the `update`
function).

-}
addCmdMsg : Cmd Msg -> ( a, Cmd Msg ) -> ( a, Cmd Msg )
addCmdMsg extraCmd ( a, cmds ) =
    ( a, Cmd.batch [ cmds, extraCmd ] )
