module Architecture.Update exposing (update)

import Architecture.Init exposing (extractWith, fromRoute)
import Architecture.Model exposing (..)
import Architecture.Msg exposing (..)
import Architecture.Route as Route exposing (Route)
import Browser.Navigation as Navigation
import Page.Classic as Classic
import Page.Home as Home
import Page.NotFound as NotFound
import Types.Session as Session exposing (Session)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( RouteChanged route, oldModel ) ->
            reroute route oldModel
                |> addCmdMsg
                    (Navigation.pushUrl
                        (.key (eject oldModel))
                        (Route.toString route)
                    )

        ( UrlChanged url, oldModel ) ->
            eject oldModel
                |> fromRoute (Route.fromUrl url)

        ( GotHomeMsg subMsg, Home subModel ) ->
            Home.update subMsg subModel
                |> extractWith Home GotHomeMsg

        ( GotNotFoundMsg subMsg, NotFound subModel ) ->
            NotFound.update subMsg subModel
                |> extractWith NotFound GotNotFoundMsg

        ( GotClassicMsg subMsg, Classic submodel ) ->
            Classic.update subMsg submodel
                |> extractWith Classic GotClassicMsg

        _ ->
            ( model, Cmd.none )


eject : Model -> Session
eject page =
    case page of
        Home model ->
            Home.eject model

        NotFound model ->
            NotFound.eject model

        Classic model ->
            Classic.eject model


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

        Route.Classic ->
            Classic.init session
                |> extractWith Classic GotClassicMsg
