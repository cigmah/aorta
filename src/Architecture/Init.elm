module Architecture.Init exposing (extractWith, fromRoute, init)

import Architecture.Model exposing (..)
import Architecture.Msg exposing (..)
import Architecture.Parser as Parser
import Architecture.Route as Route exposing (Route)
import Browser.Navigation as Navigation exposing (Key)
import Json.Decode as Decode exposing (Value)
import Page.Finish as Finish
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Note as Note
import Page.Profile as Profile
import Page.Question as Question
import Page.Revise as Revise
import Types.Session as Session exposing (Session)
import Url exposing (Url)


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    -- TODO Add a message if decoding fails
    flags
        |> Decode.decodeValue Decode.string
        |> Result.andThen (Decode.decodeString Session.decoder)
        |> Result.map (\filler -> filler key)
        |> Result.withDefault (Session.default key)
        |> fromRoute (Parser.fromUrl url)


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

        Route.Profile ->
            Profile.init session
                |> extractWith Profile GotProfileMsg

        Route.Note noteId ->
            Note.init session noteId
                |> extractWith Note GotNoteMsg

        Route.Revise ->
            Revise.init session
                |> extractWith Revise GotReviseMsg

        Route.Question questionId ->
            Question.init session questionId
                |> extractWith Question GotQuestionMsg

        Route.Finish ->
            Finish.init session
                |> extractWith Finish GotFinishMsg
