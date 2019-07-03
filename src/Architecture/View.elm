module Architecture.View exposing (view)

import Architecture.Model exposing (..)
import Architecture.Msg exposing (..)
import Architecture.Parser as Parser
import Architecture.Route as Route exposing (Route)
import Architecture.Update exposing (eject)
import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Markdown
import Page.Finish as Finish
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Note as Note
import Page.Profile as Profile
import Page.Question as Question
import Page.Revise as Revise
import Types.Credentials exposing (Auth(..))
import Types.Session as Session exposing (Session)
import Types.Styles exposing (tailwind)



-- Major View Router


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

        Question subModel ->
            Question.view subModel
                |> viewPage model GotQuestionMsg

        Finish subModel ->
            Finish.view subModel
                |> viewPage model GotFinishMsg


{-| Wrap each page's individual view function |
-}
viewPage : Model -> (subMsg -> Msg) -> Document subMsg -> Document Msg
viewPage model toMsg page =
    { title = page.title
    , body =
        page.body
            |> List.map (Html.map toMsg)
            |> wrapBody model
    }


{-| View ech navigation link |
-}
viewNavLink : { name : String, active : Bool, route : Route, icon : String, right : Bool } -> Html Msg
viewNavLink data =
    a
        [ Route.toHref data.route
        , tailwind
            [ "flex"
            , "flex-col"
            , "p-2"
            , "px-4"
            , "flex-grow"
            , "items-center"
            , "md:flex-none"
            , "md:flex-row"
            , "justify-center"
            , "cursor-pointer"
            , "hover:bg-gray-200"
            , "bg-white"
            , "hover:text-gray-700"
            , "text-sm"
            , "uppercase"
            ]
        , classList
            [ ( "text-blue-500 font-bold", data.active )
            , ( "md:ml-auto", data.right )
            ]
        ]
        [ i
            [ class "material-icons"
            , tailwind
                [ "md:pr-2" ]
            ]
            [ text data.icon ]
        , label
            [ tailwind
                [ "cursor-pointer"
                , "md:pr-2"
                ]
            ]
            [ text data.name ]
        ]


{-| View a single message |
-}
viewSingleMessage : String -> Html Msg
viewSingleMessage string =
    article
        [ class "message"
        , tailwind
            [ "bg-white"
            , "text-gray-800"
            , "p-4"
            , "fadein"
            , "shadow-lg"
            , "md:rounded"
            , "md:mb-4"
            , "cursor-pointer"
            ]
        , onClick <| ClickedMessage string
        ]
        (Markdown.toHtml Nothing string)


{-| View the list of all messages in a session |
-}
viewMessage : Session -> Html Msg
viewMessage session =
    case session.message of
        Just stringList ->
            section
                [ class "message-list"
                , tailwind
                    [ "fixed", "right-0", "top-0", "md:p-2", "md:w-1/4", "z-50" ]
                ]
                (List.map viewSingleMessage stringList)

        Nothing ->
            section [ class "hidden" ] []


{-| Wrap each page's individual body with a nav bar and messages |
-}
wrapBody : Model -> List (Html Msg) -> List (Html Msg)
wrapBody model body =
    let
        session =
            eject model

        profileText =
            case session.auth of
                User user ->
                    user.username

                Guest ->
                    "Log In"

        hideNav =
            case model of
                Question _ ->
                    True

                _ ->
                    False
    in
    [ nav
        [ tailwind
            [ "w-full"
            , "flex"
            , "fixed"
            , "bottom-0"
            , "md:top-0"
            , "md:bottom-auto"
            , "text-sm"
            , "md:text-base"
            , "items-center"
            , "z-30"
            , "text-gray-500"
            , "bg-white"
            ]
        , classList
            [ ( "hidden", hideNav ) ]
        ]
        [ div
            [ tailwind
                [ "hidden"
                , "md:block"
                , "ml-8"
                , "mr-6"
                , "text-xl"
                , "font-light"
                ]
            ]
            [ text "aorta" ]
        , viewNavLink
            { name = "Grid"
            , active = Parser.isEqual Route.Home model
            , route = Route.Home
            , icon = "notes"
            , right = False
            }
        , viewNavLink
            { name = "Revise"
            , active = Parser.isEqual Route.Revise model
            , route = Route.Revise
            , icon = "check"
            , right = False
            }
        , viewNavLink
            { name = profileText
            , active = Parser.isEqual Route.Profile model
            , route = Route.Profile
            , icon = "person"
            , right = True
            }
        ]
    ]
        ++ body
        ++ [ viewMessage (eject model) ]
