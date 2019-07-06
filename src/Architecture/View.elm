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
import Page.Elements as Elements
import Page.Finish as Finish
import Page.Home as Home
import Page.Info as Info
import Page.NotFound as NotFound
import Page.Note as Note
import Page.Profile as Profile
import Page.Question as Question
import Page.Revise as Revise
import Types.Credentials exposing (Auth(..))
import Types.Note
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

        Info subModel ->
            Info.view subModel
                |> viewPage model GotInfoMsg


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
                    "Info"

        hideNav =
            case model of
                Question _ ->
                    True

                _ ->
                    False

        searchBarData =
            { webDataResponse = session.searchResults
            , responseDataToResult = Types.Note.toSearchResult
            , forMobile = False
            , inputData =
                { value = session.searchInput
                , onInput = ChangedSearchInput
                , placeholder = "Search notes..."
                , type_ = "search"
                }
            }
    in
    Elements.navBar hideNav
        [ { name = "Grid"
          , active = Parser.isEqual Route.Home model
          , route = Route.Home
          , icon = "notes"
          , hideOnMobile = True
          , hideOnDesktop = False
          }
        , { name = "Search"
          , active = Parser.isEqual Route.Home model
          , route = Route.Home
          , icon = "search"
          , hideOnMobile = False
          , hideOnDesktop = True
          }
        , { name = "Test"
          , active = Parser.isEqual Route.Revise model
          , route = Route.Revise
          , icon = "check"
          , hideOnMobile = False
          , hideOnDesktop = False
          }
        ]
        searchBarData
        [ { name = "Info"
          , active = Parser.isEqual Route.Info model
          , route = Route.Info
          , icon = "info"
          , hideOnMobile = False
          , hideOnDesktop = False
          }
        , { name = profileText
          , active = Parser.isEqual Route.Profile model
          , route = Route.Profile
          , icon = "person"
          , hideOnMobile = False
          , hideOnDesktop = False
          }
        ]
        :: Elements.messages session ClickedMessage
        :: body
