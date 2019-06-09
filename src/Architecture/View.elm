module Architecture.View exposing (view)

import Architecture.Model exposing (..)
import Architecture.Msg exposing (..)
import Browser exposing (Document)
import Html exposing (..)
import Page.Home as Home
import Page.NotFound as NotFound


view : Model -> Document Msg
view model =
    case model of
        Home subModel ->
            Home.view subModel
                |> viewPage GotHomeMsg

        NotFound session ->
            NotFound.view session
                |> viewPage GotNotFoundMsg


viewPage : (subMsg -> Msg) -> Document subMsg -> Document Msg
viewPage toMsg page =
    { title = page.title
    , body = List.map (Html.map toMsg) page.body
    }
