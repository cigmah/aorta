module App exposing (main)

{-| This is the main frontend application entry-point for AORTA.

The architecture is a single-page application, with each component of the
architecture stored in `./Architecture/` as a separate elm file.

-}

import Architecture.Init exposing (init)
import Architecture.Model exposing (Model)
import Architecture.Msg exposing (Msg(..))
import Architecture.Subscriptions exposing (subscriptions)
import Architecture.Update exposing (update)
import Architecture.View exposing (view)
import Browser
import Json.Decode exposing (Value)


{-| The main app entry point.

The type annotation indicates that the program takes an initialisation flag
of type `Json.Decode.Value` (which is passed to the `init` function) and
produces a `view` on the `Model` that emits a `Msg` type.

-}
main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
