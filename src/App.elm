module App exposing (main)

import Architecture.Init exposing (init)
import Architecture.Model exposing (..)
import Architecture.Msg exposing (..)
import Architecture.Subscriptions exposing (subscriptions)
import Architecture.Update exposing (update)
import Architecture.View exposing (view)
import Browser


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
