module Architecture.View exposing (view)

{-| Contains the top-level application `view` function.

The `view` function takes an immutable "view" of the model, and outputs HTML
that can emit events of the `Msg` type. All information that needs to be
dynamically rendered _must_ be in the `Model` that is passed to the `view`
function.

-}

import Architecture.Model exposing (Model(..))
import Architecture.Msg exposing (Msg(..))
import Browser exposing (Document)
import Html
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Objective as Objective
import Page.ObjectiveList as ObjectiveList
import Page.Profile as Profile
import Page.Question as Question
import Page.Report as Report
import Types.Credentials exposing (Auth(..))


{-| The application top-level `view` function.

Each page specifies it's own view as part of it's individual
Model-View-Update architecture. This function simply maps those individual
page views to the application top-level.

-}
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

        Objective subModel ->
            Objective.view subModel
                |> viewPage model GotObjectiveMsg

        ObjectiveList subModel ->
            ObjectiveList.view subModel
                |> viewPage model GotObjectiveListMsg

        Question subModel ->
            Question.view subModel
                |> viewPage model GotQuestionMsg

        Report subModel ->
            Report.view subModel
                |> viewPage model GotReportMsg


{-| Wraps a page's title and body.

This function wraps a page's title and body into the top-level. If something
should be applied to every page, such as a navigation bar, then it can be
added into the pipeline here.

-}
viewPage : Model -> (subMsg -> Msg) -> Document subMsg -> Document Msg
viewPage _ toMsg page =
    let
        title =
            page.title

        body =
            page.body
                |> List.map (Html.map toMsg)
    in
    { title = title
    , body = body
    }
