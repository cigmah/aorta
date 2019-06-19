module Architecture.Update exposing (update)

import Architecture.Init exposing (extractWith, fromRoute)
import Architecture.Model exposing (..)
import Architecture.Msg exposing (..)
import Architecture.Route as Route exposing (Route)
import Browser
import Browser.Navigation as Navigation
import Page.Home as Home
import Page.NotFound as NotFound
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

        ( UrlChanged url, oldModel ) ->
            eject oldModel
                |> fromRoute (Route.fromUrl url)

        ( GotHomeMsg subMsg, Home subModel ) ->
            Home.update subMsg subModel
                |> extractWith Home GotHomeMsg

        ( GotNotFoundMsg subMsg, NotFound subModel ) ->
            NotFound.update subMsg subModel
                |> extractWith NotFound GotNotFoundMsg

        ( GotQuestionsMsg subMsg, Questions submodel ) ->
            Questions.update subMsg submodel
                |> extractWith Questions GotQuestionsMsg

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


addCmdMsg : Cmd Msg -> ( a, Cmd Msg ) -> ( a, Cmd Msg )
addCmdMsg extraCmd ( a, cmds ) =
    ( a, Cmd.batch [ cmds, extraCmd ] )


reroute : Route -> Model -> ( Model, Cmd Msg )
reroute route model =
    let
        session =
            eject model
    in
    case route of
        Route.Home ->
            Home.init session
                |> extractWith Home GotHomeMsg

        Route.NotFound ->
            NotFound.init session
                |> extractWith NotFound GotNotFoundMsg

        Route.Questions ->
            Questions.init session
                |> extractWith Questions GotQuestionsMsg
