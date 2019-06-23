module Architecture.Msg exposing (Msg(..))

import Architecture.Route as Route exposing (Route)
import Browser exposing (UrlRequest)
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Note as Note
import Page.Profile as Profile
import Page.Question as Question
import Url exposing (Url)


type Msg
    = UrlRequested UrlRequest
    | UrlChanged Url
    | RouteChanged Route
    | ClearMessages
    | GotHomeMsg Home.Msg
    | GotNotFoundMsg NotFound.Msg
    | GotQuestionMsg Question.Msg
    | GotProfileMsg Profile.Msg
    | GotNoteMsg Note.Msg
