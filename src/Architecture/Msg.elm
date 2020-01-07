module Architecture.Msg exposing (Msg(..))

{-| Contains the basic `Msg` type for the application.

The `Msg` type enumerates all possible actions or events that can be emitted
from the application. Any events - such as click events, HTTP requests etc. -
must be described by a variant of the `Msg` type.

-}

import Architecture.Route exposing (Route)
import Browser exposing (UrlRequest)
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Objective as Objective
import Page.ObjectiveList as ObjectiveList
import Page.Profile as Profile
import Page.Question as Question
import Page.Report as Report
import RemoteData exposing (WebData)
import Types.Credentials exposing (Credentials)
import Types.Register as Register
import Url exposing (Url)


{-| The application's `Msg` type.

The `Msg` type is modelled as a discriminated union (as usual). Some `Msg`
variants are top-level application messages, which should trigger the same
response regardless of the page (e.g. if the Url is changed). These messages
_cannot_ be accessed from within each page's model, so must be emitted only
\*from views created by the top-level view.

The remainder of the message variants are the tagged messages from each
individual page's architecture, which brings each page's `Msg` up to the top
level.

-}
type Msg
    = UrlRequested UrlRequest
    | UrlChanged Url
    | RouteChanged Route
    | ClickedMessage String
    | GotServiceWorkerNotification ()
      -- Logging in and Registering is done at the top-level application
    | ToggledShowLogin
    | ToggledAuthDialogType
    | ClickedLogout
      -- Login messages
    | ChangedLoginUsername String
    | ChangedLoginPassword String
    | ClickedLogin
    | GotLoginResponse (WebData Credentials)
      -- Registration messages
    | ChangedRegisterUsername String
    | ChangedRegisterEmail String
    | ClickedRegister
    | GotRegisterResponse (WebData Register.Response)
      -- Page messages
    | GotHomeMsg Home.Msg
    | GotNotFoundMsg NotFound.Msg
    | GotProfileMsg Profile.Msg
    | GotObjectiveMsg Objective.Msg
    | GotObjectiveListMsg ObjectiveList.Msg
    | GotQuestionMsg Question.Msg
    | GotReportMsg Report.Msg
