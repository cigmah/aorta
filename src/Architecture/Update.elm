module Architecture.Update exposing (update)

import Architecture.Init exposing (extractWith)
import Architecture.Model exposing (..)
import Architecture.Msg exposing (..)
import Page.Home as Home
import Page.NotFound as NotFound


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotHomeMsg subMsg, Home subModel ) ->
            Home.update subMsg subModel
                |> extractWith Home GotHomeMsg

        ( GotNotFoundMsg subMsg, NotFound subModel ) ->
            NotFound.update subMsg subModel
                |> extractWith NotFound GotNotFoundMsg

        _ ->
            ( model, Cmd.none )
