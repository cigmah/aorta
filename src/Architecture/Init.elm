module Architecture.Init exposing (extractWith, fromRoute, init)

{-| Contains basic functions relating to initialising the application.

When the application is loaded, data from the surrounding context (e.g. the
URL, or data stored in local storage) needs to be parsed and loaded. This is
the primary responsibility of this module.

-}

import Architecture.Model exposing (Model(..))
import Architecture.Msg exposing (Msg(..))
import Architecture.Parser as Parser
import Architecture.Route as Route exposing (Route)
import Browser.Navigation exposing (Key)
import Json.Decode as Decode exposing (Value)
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Objective as Objective
import Page.ObjectiveList as ObjectiveList
import Page.Profile as Profile
import Page.Question as Question
import Page.Report as Report
import Types.Session as Session exposing (Session)
import Url exposing (Url)


{-| The main initialisation function for the application.

When the application is first loaded, data from local storage is passed as a
flag to the application. The flag is then decoded to provide "cached" session
information to the application, or a sensible default if it does not exist.

The initial model is then loaded by parsing the URL and passing it the
session that was either loaded from local storage or created from the default
session.

-}
init : Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    flags
        |> Decode.decodeValue Decode.string
        |> Result.andThen (Decode.decodeString Session.decoder)
        |> Result.map (\filler -> filler key)
        |> Result.withDefault (Session.default key)
        |> fromRoute (Parser.fromUrl url)


{-| Converts a page model and update into an application model and update.

The top-level application `Model` is a discriminated union (tagged enum), with
each variant representing a separate page. Each page maintains its own
Model-View-Update architecture. In order to integrate each page's separate
architecture into the top-level application, the page's model and updates
need to be mapped to the application's model and updates.

This function converts each page's separate architecture into a top-level
application architecture by "extracting" its model and updates.

-}
extractWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
extractWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel, Cmd.map toMsg subCmd )


{-| Initialises a page model from route and a provided session object.

When a page is initialised (either from initialisation from the app, or from
navigation), the page's own `init` function needs to be called to produce its
model and updates, then extracted into the top-level application.

This function maps a route and a provided session into a new initialised
top-level application.

-}
fromRoute : Route -> Session -> ( Model, Cmd Msg )
fromRoute route session =
    case route of
        Route.Home ->
            Home.init session
                |> extractWith Home GotHomeMsg

        Route.NotFound ->
            NotFound.init session
                |> extractWith NotFound GotNotFoundMsg

        Route.Profile ->
            Profile.init session
                |> extractWith Profile GotProfileMsg

        Route.Objective objectiveId ->
            Objective.init session objectiveId
                |> extractWith Objective GotObjectiveMsg

        Route.ObjectiveList objectiveListQueries ->
            ObjectiveList.init session objectiveListQueries
                |> extractWith ObjectiveList GotObjectiveListMsg

        Route.Question questionId ->
            Question.init session questionId
                |> extractWith Question GotQuestionMsg

        Route.Report ->
            Report.init session
                |> extractWith Report GotReportMsg
