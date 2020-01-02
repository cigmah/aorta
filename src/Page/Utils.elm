module Page.Utils exposing (..)

{-| General utilities and helpers shared across all pages.
-}


{-| Wrap a model in no side effect command. A convenience helper.
-}
withCmdNone : model -> ( model, Cmd msg )
withCmdNone model =
    ( model, Cmd.none )


{-| Wrap a model in a command. A convenience helper.
-}
withCmd : Cmd msg -> model -> ( model, Cmd msg )
withCmd cmd model =
    ( model, cmd )
