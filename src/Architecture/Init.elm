module Architecture.Init exposing (extractWith, fromRoute, init)

import Architecture.Model exposing (..)
import Architecture.Msg exposing (..)
import Architecture.Route as Route exposing (Route)
import Browser.Navigation as Navigation exposing (Key)
import Json.Decode as Decode exposing (Value)
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Note as Note
import Page.Profile as Profile
import Page.Questions as Questions
import Types.Credentials exposing (Auth(..))
import Types.Session as Session exposing (Session)
import Url exposing (Url)


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        debugging =
            flags
                |> Decode.decodeValue Decode.string
                |> Result.andThen (Decode.decodeString Session.decoder)
                |> Result.map (\filler -> filler key)

        _ =
            Debug.log "debugging" debugging
    in
    -- TODO Add a message if decoding fails
    flags
        |> Decode.decodeValue Decode.string
        |> Result.andThen (Decode.decodeString Session.decoder)
        |> Result.map (\filler -> filler key)
        |> Result.withDefault (Session.default key)
        |> fromRoute (Route.fromUrl url)


extractWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
extractWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel, Cmd.map toMsg subCmd )


fromRoute : Route -> Session -> ( Model, Cmd Msg )
fromRoute route session =
    case route of
        Route.Home ->
            Home.init session
                |> extractWith Home GotHomeMsg

        Route.NotFound ->
            NotFound.init session
                |> extractWith NotFound GotNotFoundMsg

        Route.Questions ->
            Questions.init session
                |> extractWith Questions GotQuestionsMsg

        Route.Profile ->
            Profile.init session
                |> extractWith Profile GotProfileMsg

        Route.Note noteId ->
            Note.init session noteId
                |> extractWith Note GotNoteMsg
