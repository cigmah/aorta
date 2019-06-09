module Architecture.Msg exposing (Msg(..))

import Browser exposing (UrlRequest)
import Page.Home as Home
import Page.NotFound as NotFound
import Url exposing (Url)


type Msg
    = UrlRequested UrlRequest
    | UrlChanged Url
    | GotHomeMsg Home.Msg
    | GotNotFoundMsg NotFound.Msg
