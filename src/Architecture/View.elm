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

        Profile subModel ->
            Profile.view subModel
                |> viewPage model GotProfileMsg

        Note subModel ->
            Note.view subModel
                |> viewPage model GotNoteMsg


viewPage : Model -> (subMsg -> Msg) -> Document subMsg -> Document Msg
viewPage model toMsg page =
    { title = page.title
    , body =
        List.map (Html.map toMsg) page.body
            |> wrapBody model
    }


viewNavLink : { name : String, active : Bool, route : Route, icon : String } -> Html Msg
viewNavLink data =
    a
        [ Route.toHref data.route
        , classList [ ( "active", data.active ) ]
        ]
        [ i [ class "material-icons" ] [ text data.icon ]
        , label [] [ text data.name ]
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


isRouteEqual : Route -> Model -> Bool
isRouteEqual route model =
    case ( route, model ) of
        ( Route.Home, Home _ ) ->
            True

        ( Route.Profile, Profile _ ) ->
            True

        ( Route.Note _, Note _ ) ->
            True

        _ ->
            False


wrapBody : Model -> List (Html Msg) -> List (Html Msg)
wrapBody model body =
    [ nav
        [ classList [ ( "hidden", isRouteEqual (Route.Note 0) model ) ] ]
        [ viewNavLink
            { name = "Matrix"
            , active = isRouteEqual Route.Home model
            , route = Route.Home
            , icon = "notes"
            }
        , viewNavLink
            { name = "Profile"
            , active = isRouteEqual Route.Profile model
            , route = Route.Profile
            , icon = "person"
            }
        ]
    , viewMessage (eject model)
    ]
        ++ body
