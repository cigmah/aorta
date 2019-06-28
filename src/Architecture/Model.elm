module Architecture.Model exposing (Model(..))

import Page.Home as Home
import Page.NotFound as NotFound
import Page.Note as Note
import Page.Profile as Profile
import Page.Revise as Revise
import Types.Session exposing (Session)


type Model
    = Home Home.Model
    | NotFound NotFound.Model
    | Profile Profile.Model
    | Note Note.Model
    | Revise Revise.Model
