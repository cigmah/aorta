module Architecture.Subscriptions exposing (subscriptions)

import Architecture.Model exposing (..)
import Architecture.Msg exposing (..)
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Questions as Questions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Home subModel ->
            Sub.map GotHomeMsg (Home.subscriptions subModel)

        NotFound subModel ->
            Sub.map GotNotFoundMsg (NotFound.subscriptions subModel)

        Questions subModel ->
            Sub.map GotQuestionsMsg (Questions.subscriptions subModel)
