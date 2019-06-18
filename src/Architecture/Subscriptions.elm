module Architecture.Subscriptions exposing (subscriptions)

import Architecture.Model exposing (..)
import Architecture.Msg exposing (..)
import Page.Classic as Classic
import Page.Home as Home
import Page.NotFound as NotFound


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Home subModel ->
            Sub.map GotHomeMsg (Home.subscriptions subModel)

        NotFound subModel ->
            Sub.map GotNotFoundMsg (NotFound.subscriptions subModel)

        Classic subModel ->
            Sub.map GotClassicMsg (Classic.subscriptions subModel)
