module Architecture.View exposing (view)

import Architecture.Model exposing (..)
import Architecture.Msg exposing (..)
import Architecture.Route as Route
import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Page.Classic as Classic
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

        Classic subModel ->
            Classic.view subModel
                |> viewPage GotClassicMsg


viewPage : (subMsg -> Msg) -> Document subMsg -> Document Msg
viewPage toMsg page =
    { title = page.title
    , body = List.map (Html.map toMsg) page.body |> wrapBody
    }


wrapBody : List (Html Msg) -> List (Html Msg)
wrapBody body =
    [ main_ [] (navMenu :: body) ]


navMenu : Html Msg
navMenu =
    nav []
        [ div [ class "icon" ]
            [ img [ class "logo", src "./logo.svg" ] []
            , text "AORTA"
            ]
        , div
            [ class "link"
            , onClick (RouteChanged Route.Home)
            ]
            [ text "Home" ]
        , div [ class "link" ]
            [ text "About" ]
        , div [ class "link" ]
            [ text "Pinboard" ]
        , div
            [ class "link right" ]
            [ text "Login" ]
        ]
