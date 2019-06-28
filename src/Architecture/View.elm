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
import Page.Revise as Revise
import Secret exposing (baseUrl)
import Types.Session as Session exposing (Session)
import Types.Styles exposing (tailwind)


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

        Revise subModel ->
            Revise.view subModel
                |> viewPage model GotReviseMsg


viewPage : Model -> (subMsg -> Msg) -> Document subMsg -> Document Msg
viewPage model toMsg page =
    { title = page.title
    , body =
        List.map (Html.map toMsg) page.body
            |> wrapBody model
    }


viewNavLink : { name : String, active : Bool, route : Route, icon : String, right : Bool } -> Html Msg
viewNavLink data =
    a
        [ Route.toHref data.route
        , tailwind
            [ "flex"
            , "flex-col"
            , "p-2"
            , "flex-grow"
            , "items-center"
            , "md:flex-none"
            , "md:flex-row"
            , "justify-center"
            , "cursor-pointer"
            , "hover:bg-white"
            , "hover:text-blue-800"
            ]
        , classList
            [ ( "active", data.active )
            , ( "md:ml-auto", data.right )
            ]
        ]
        [ i
            [ class "material-icons"
            , tailwind
                [ "px-2" ]
            ]
            [ text data.icon ]
        , label [ tailwind [ "cursor-pointer" ] ] [ text data.name ]
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

        ( Route.Revise, Revise _ ) ->
            True

        _ ->
            False


wrapBody : Model -> List (Html Msg) -> List (Html Msg)
wrapBody model body =
    [ nav
        [ tailwind
            [ "w-full"
            , "bg-blue-800"
            , "text-white"
            , "fixed"
            , "flex"
            , "bottom-0"
            , "md:top-0"
            , "md:bottom-auto"
            , "text-xs"
            , "md:text-sm"
            , "items-center"
            , "z-50"
            ]
        , classList [ ( "hidden", isRouteEqual (Route.Note 0) model ) ]
        ]
        [ img
            [ src "./icon.svg"
            , tailwind
                [ "h-6"
                , "w-6"
                , "ml-4"
                , "hidden"
                , "md:block"
                ]
            ]
            []
        , div
            [ tailwind
                [ "text-white"
                , "font-bold"
                , "hidden"
                , "md:block"
                , "ml-4"
                , "mr-6"
                , "text-lg"
                ]
            ]
            [ text "AORTA" ]
        , viewNavLink
            { name = "Matrix"
            , active = isRouteEqual Route.Home model
            , route = Route.Home
            , icon = "notes"
            , right = False
            }
        , viewNavLink
            { name = "Revise"
            , active = isRouteEqual Route.Revise model
            , route = Route.Revise
            , icon = "check"
            , right = False
            }
        , viewNavLink
            { name = "Profile"
            , active = isRouteEqual Route.Profile model
            , route = Route.Profile
            , icon = "person"
            , right = True
            }
        ]
    , viewMessage (eject model)
    ]
        ++ body
