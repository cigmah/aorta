module Architecture.Update exposing (eject, update)

import Architecture.Init as Init exposing (extractWith)
import Architecture.Model exposing (..)
import Architecture.Msg exposing (..)
import Architecture.Route as Route exposing (Route)
import Browser
import Browser.Navigation as Navigation
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Note as Note
import Page.Profile as Profile
import Page.Questions as Questions
import Types.Session as Session exposing (Session)
import Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( RouteChanged route, _ ) ->
            reroute route model
                |> addCmdMsg
                    (Navigation.pushUrl
                        (.key (eject model))
                        (Route.toString route)
                    )

        ( UrlRequested urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal internal ->
                    ( model
                    , Navigation.pushUrl
                        (.key (eject model))
                        (Url.toString internal)
                    )

                Browser.External external ->
                    ( model, Navigation.load external )

        ( UrlChanged url, _ ) ->
            eject model
                |> Init.fromRoute (Route.fromUrl url)

        ( ClearMessages, _ ) ->
            eject model
                |> Session.clearMessages
                |> inject model

        ( GotHomeMsg subMsg, Home subModel ) ->
            Home.update subMsg subModel
                |> extractWith Home GotHomeMsg

        ( GotNotFoundMsg subMsg, NotFound subModel ) ->
            NotFound.update subMsg subModel
                |> extractWith NotFound GotNotFoundMsg

        ( GotQuestionsMsg subMsg, Questions submodel ) ->
            Questions.update subMsg submodel
                |> extractWith Questions GotQuestionsMsg

        ( GotProfileMsg subMsg, Profile subModel ) ->
            Profile.update subMsg subModel
                |> extractWith Profile GotProfileMsg

        ( GotNoteMsg subMsg, Note subModel ) ->
            Note.update subMsg subModel
                |> extractWith Note GotNoteMsg

        _ ->
            ( model, Cmd.none )


eject : Model -> Session
eject page =
    case page of
        Home model ->
            Home.eject model

        NotFound model ->
            NotFound.eject model

        Questions model ->
            Questions.eject model

        Profile model ->
            Profile.eject model

        Note model ->
            Note.eject model


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

        Questions model ->
            session
                |> Questions.inject model
                |> extractWith Questions GotQuestionsMsg

        Profile model ->
            session
                |> Profile.inject model
                |> extractWith Profile GotProfileMsg

        Note model ->
            session
                |> Note.inject model
                |> extractWith Note GotNoteMsg


reroute : Route -> Model -> ( Model, Cmd Msg )
reroute route model =
    eject model
        |> Init.fromRoute route


addCmdMsg : Cmd Msg -> ( a, Cmd Msg ) -> ( a, Cmd Msg )
addCmdMsg extraCmd ( a, cmds ) =
    ( a, Cmd.batch [ cmds, extraCmd ] )
