module Architecture.Update exposing (eject, update)

import Architecture.Init as Init exposing (extractWith)
import Architecture.Model exposing (..)
import Architecture.Msg exposing (..)
import Architecture.Route as Route exposing (Route)
import Browser
import Browser.Navigation as Navigation
import Page.Finish as Finish
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Note as Note
import Page.Profile as Profile
import Page.Question as Question
import Page.Revise as Revise
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

        ( GotProfileMsg subMsg, Profile subModel ) ->
            Profile.update subMsg subModel
                |> extractWith Profile GotProfileMsg

        ( GotNoteMsg subMsg, Note subModel ) ->
            Note.update subMsg subModel
                |> extractWith Note GotNoteMsg

        ( GotReviseMsg subMsg, Revise subModel ) ->
            Revise.update subMsg subModel
                |> extractWith Revise GotReviseMsg

        ( GotQuestionMsg subMsg, Question subModel ) ->
            Question.update subMsg subModel
                |> extractWith Question GotQuestionMsg

        ( GotFinishMsg subMsg, Finish subModel ) ->
            Finish.update subMsg subModel
                |> extractWith Finish GotFinishMsg

        _ ->
            ( model, Cmd.none )


eject : Model -> Session
eject page =
    case page of
        Home model ->
            Home.eject model

        NotFound model ->
            NotFound.eject model

        Profile model ->
            Profile.eject model

        Note model ->
            Note.eject model

        Revise model ->
            Revise.eject model

        Question model ->
            Question.eject model

        Finish model ->
            Finish.eject model


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

        Note model ->
            session
                |> Note.inject model
                |> extractWith Note GotNoteMsg

        Revise model ->
            session
                |> Revise.inject model
                |> extractWith Revise GotReviseMsg

        Question model ->
            session
                |> Question.inject model
                |> extractWith Question GotQuestionMsg

        Finish model ->
            session
                |> Finish.inject model
                |> extractWith Finish GotFinishMsg


reroute : Route -> Model -> ( Model, Cmd Msg )
reroute route model =
    eject model
        |> Init.fromRoute route


addCmdMsg : Cmd Msg -> ( a, Cmd Msg ) -> ( a, Cmd Msg )
addCmdMsg extraCmd ( a, cmds ) =
    ( a, Cmd.batch [ cmds, extraCmd ] )
