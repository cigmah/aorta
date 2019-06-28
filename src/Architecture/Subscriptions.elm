module Architecture.Subscriptions exposing (subscriptions)

import Architecture.Model exposing (..)
import Architecture.Msg exposing (..)
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Note as Note
import Page.Profile as Profile
import Page.Revise as Revise


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Home subModel ->
            Sub.map GotHomeMsg (Home.subscriptions subModel)

        NotFound subModel ->
            Sub.map GotNotFoundMsg (NotFound.subscriptions subModel)

        Profile subModel ->
            Sub.map GotProfileMsg (Profile.subscriptions subModel)

        Note subModel ->
            Sub.map GotNoteMsg (Note.subscriptions subModel)

        Revise subModel ->
            Sub.map GotReviseMsg (Revise.subscriptions subModel)
