module Architecture.Msg exposing (Msg(..))

import Architecture.Route as Route exposing (Route)
import Browser exposing (UrlRequest)
import Page.Finish as Finish
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Note as Note
import Page.Profile as Profile
import Page.Question as Question
import Page.Revise as Revise
import RemoteData exposing (RemoteData(..), WebData)
import Types.Note
import Url exposing (Url)


type Msg
    = UrlRequested UrlRequest
    | UrlChanged Url
    | RouteChanged Route
    | ClickedMessage String
    | ChangedSearchInput String
    | GotSearchResults (WebData (List Types.Note.ListData))
    | GotHomeMsg Home.Msg
    | GotNotFoundMsg NotFound.Msg
    | GotProfileMsg Profile.Msg
    | GotNoteMsg Note.Msg
    | GotReviseMsg Revise.Msg
    | GotQuestionMsg Question.Msg
    | GotFinishMsg Finish.Msg
