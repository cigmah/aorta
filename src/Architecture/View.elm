module Architecture.View exposing (view)

import Architecture.Model exposing (..)
import Architecture.Msg exposing (..)
import Architecture.Route as Route exposing (Route)
import Architecture.Update exposing (eject)
import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Markdown
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Note as Note
import Page.Profile as Profile
import Page.Questions as Questions
import Secret exposing (baseUrl)
import Types.Session as Session exposing (Session)


view : Model -> Document Msg
view model =
    case model of
        Home subModel ->
            Home.view subModel
                |> viewPage model GotHomeMsg

        NotFound session ->
            NotFound.view session
                |> viewPage model GotNotFoundMsg

        Questions subModel ->
            Questions.view subModel
                |> viewPage model GotQuestionsMsg

        Profile subModel ->
            Profile.view subModel
                |> viewPage model GotProfileMsg

        Note subModel ->
            Note.view subModel
                |> viewPage model GotNoteMsg


viewPage : Model -> (subMsg -> Msg) -> Document subMsg -> Document Msg
viewPage model toMsg page =
    { title = page.title
    , body = List.map (Html.map toMsg) page.body |> wrapBody model
    }


viewNavLink : { name : String, active : Bool, route : Route, icon : String } -> Html Msg
viewNavLink data =
    li []
        [ a [ Route.toHref data.route ]
            [ i [ class "material-icons" ] [ text data.icon ]
            , label [] [ text data.name ]
            ]
        ]


viewSingleMessage : String -> Html Msg
viewSingleMessage string =
    article [ class "message" ]
        (Markdown.toHtml Nothing string)


viewMessage : Session -> Html Msg
viewMessage session =
    case session.message of
        Just stringList ->
            section [ class "message-list", onClick ClearMessages ]
                (List.map viewSingleMessage stringList)

        Nothing ->
            section [ class "hidden" ] []


wrapBody : Model -> List (Html Msg) -> List (Html Msg)
wrapBody model body =
    [ nav []
        [ ul []
            [ viewNavLink { name = "Search", active = False, route = Route.Home, icon = "search" }
            , viewNavLink { name = "Revise", active = False, route = Route.Questions, icon = "notes" }
            , viewNavLink { name = "Profile", active = False, route = Route.Profile, icon = "person" }
            ]
        ]
    , viewMessage (eject model)
    ]
        ++ body
