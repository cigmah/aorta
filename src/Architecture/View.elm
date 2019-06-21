module Architecture.View exposing (view)

import Architecture.Model exposing (..)
import Architecture.Msg exposing (..)
import Architecture.Route as Route exposing (Route)
import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Profile as Profile
import Page.Questions as Questions
import Secret exposing (baseUrl)


view : Model -> Document Msg
view model =
    case model of
        Home subModel ->
            Home.view subModel
                |> viewPage GotHomeMsg

        NotFound session ->
            NotFound.view session
                |> viewPage GotNotFoundMsg

        Questions subModel ->
            Questions.view subModel
                |> viewPage GotQuestionsMsg

        Profile subModel ->
            Profile.view subModel
                |> viewPage GotProfileMsg


viewPage : (subMsg -> Msg) -> Document subMsg -> Document Msg
viewPage toMsg page =
    { title = page.title
    , body = List.map (Html.map toMsg) page.body |> wrapBody
    }


viewNavLink : { name : String, active : Bool, route : Route, icon : String } -> Html Msg
viewNavLink data =
    li []
        [ a [ Route.toHref data.route ]
            [ i [ class "material-icons" ] [ text data.icon ]
            , label [] [ text data.name ]
            ]
        ]


wrapBody : List (Html Msg) -> List (Html Msg)
wrapBody body =
    nav []
        [ ul []
            [ viewNavLink { name = "Search", active = False, route = Route.Home, icon = "search" }
            , viewNavLink { name = "Revise", active = False, route = Route.Questions, icon = "notes" }
            , viewNavLink { name = "Profile", active = False, route = Route.Profile, icon = "person" }
            ]
        ]
        :: body
