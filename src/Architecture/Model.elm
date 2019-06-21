module Architecture.Model exposing (Model(..))

import Page.Home as Home
import Page.NotFound as NotFound
import Page.Profile as Profile
import Page.Questions as Questions
import Types.Session exposing (Session)


type Model
    = Home Home.Model
    | NotFound NotFound.Model
    | Questions Questions.Model
    | Profile Profile.Model
