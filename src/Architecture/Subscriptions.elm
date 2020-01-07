port module Architecture.Subscriptions exposing (subscriptions)

{-| Contains the subscriptions for the top-level application.

Subscriptions are events invoked by the browser to send a message, such as at
every time interval or frame.

There are currently no top-level subscriptions, so this module simply maps
page subscriptions into the top-level application.

-}

import Architecture.Model exposing (Model(..))
import Architecture.Msg exposing (Msg(..))
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Objective as Objective
import Page.ObjectiveList as ObjectiveList
import Page.Profile as Profile
import Page.Question as Question
import Page.Report as Report


{-| Maps a `Model` into the correct page subscriptions.

This function simply takes the `Model`, and returns the correct subscriptions
for the page the `Model` is currently on.

-}
subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Home subModel ->
            {- Only add the service worker notification for the home page. -}
            Sub.batch
                [ Sub.map GotHomeMsg (Home.subscriptions subModel)
                , serviceWorkerNotification GotServiceWorkerNotification
                ]

        NotFound subModel ->
            Sub.map GotNotFoundMsg (NotFound.subscriptions subModel)

        Profile subModel ->
            Sub.map GotProfileMsg (Profile.subscriptions subModel)

        Objective subModel ->
            Sub.map GotObjectiveMsg (Objective.subscriptions subModel)

        ObjectiveList subModel ->
            Sub.map GotObjectiveListMsg (ObjectiveList.subscriptions subModel)

        Question subModel ->
            Sub.map GotQuestionMsg (Question.subscriptions subModel)

        Report subModel ->
            Sub.map GotReportMsg (Report.subscriptions subModel)


{-| A port which susbcribes to notification from the service worker that new content is available.
-}
port serviceWorkerNotification : (() -> msg) -> Sub msg
