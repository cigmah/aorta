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


viewPage : (subMsg -> Msg) -> Document subMsg -> Document Msg
viewPage toMsg page =
    { title = page.title
    , body = List.map (Html.map toMsg) page.body |> wrapBody
    }


viewNavLink : { name : String, active : Bool, route : Route } -> Html Msg
viewNavLink data =
    a
        [ Route.toHref data.route
        , class "flex-auto pt-2 pb-2 cursor-pointer"
        , classList
            [ ( "text-gray-200 bg-gray-800", data.active )
            , ( "hover:bg-gray-400 hover:text-gray-900", not data.active )
            ]
        ]
        [ text data.name ]


wrapBody : List (Html Msg) -> List (Html Msg)
wrapBody body =
    [ header
        [ class "fixed w-screen shadow bg-gray-200 text-gray-600 z-10" ]
        [ nav
            [ class "flex justify-between font-bold text-center" ]
            [ viewNavLink { name = "Home", active = False, route = Route.Home }
            , viewNavLink { name = "EMQs", active = False, route = Route.Questions }
            , viewNavLink { name = "Cases", active = False, route = Route.NotFound }
            , viewNavLink { name = "Settings", active = False, route = Route.NotFound }
            ]
        ]
    , main_
        [ class "flex sm:justify-center sm:bg-gray-100 min-h-screen flex-col items-center sm:py-4" ]
        [ article
            [ class "sm:m-8 max-w-xl sm:shadow-lg rounded mt-10 mb-8 relative bg-white" ]
            body
        ]
    , footer
        [ class "bg-gray-200 hidden sm:flex text-gray-500 text-sm px-4 py-2 fixed bottom-0 right-0" ]
        [ p
            [ class "w-screen text-right" ]
            [ text "This is a project by "
            , a [ class "underline", href "https://cigmah.github.io/" ] [ text "CIGMAH" ]
            , text " open sourced under under GPLv3. "
            , a [ class "underline", href "https://github.com/cigmah/aorta" ] [ text "The source code is available on GitHub." ]
            ]
        ]
    ]
