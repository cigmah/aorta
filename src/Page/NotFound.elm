module Page.NotFound exposing (Model, Msg, eject, init, subscriptions, update, view)

import Browser exposing (Document)
import Types.Session exposing (Session)


type alias Model =
    {}


type Msg
    = Test


init : Session -> ( Model, Cmd Msg )
init session =
    ( {}, Cmd.none )


eject : Model -> Session
eject model =
    {}


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( {}, Cmd.none )


view : Model -> Document Msg
view model =
    { title = ""
    , body = []
    }
