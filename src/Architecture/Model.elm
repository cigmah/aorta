module Architecture.Model exposing (Model(..))

import Page.Classic as Classic
import Page.Home as Home
import Page.NotFound as NotFound
import Types.Session exposing (Session)


type Model
    = Home Home.Model
    | NotFound NotFound.Model
    | Classic Classic.Model
