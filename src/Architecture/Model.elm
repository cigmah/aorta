module Architecture.Model exposing (Model(..))

{-| Contains the base `Model` type.

The application `Model` type stores _all_ data that the application needs to
run. The `view` of the `Model` can only access dynamic information from this
data type.

-}

import Page.Home as Home
import Page.NotFound as NotFound
import Page.Objective as Objective
import Page.ObjectiveList as ObjectiveList
import Page.Profile as Profile
import Page.Question as Question
import Page.Report as Report


{-| The central application `Model` type.

The central application `Model` type is modelled as a discriminated union,
where each variant represents a separate page's `Model`. Each page implements
it's own mini Elm architecture with it's own separate model, which keeps most
of the logic contained within each page.

There is some data that needs to be shared between pages during a user's
session. This is stored in a `Session` object, which each separate page
`Model` has a dedicated space for, and is passed around when each page is
initialised or navigated to. **All** information that needs to be shareable
between pages _must_ be stored in this `Session` object, as pages cannot
otherwise alter the data or behaviour of other pages.

-}
type Model
    = Home Home.Model
    | NotFound NotFound.Model
    | Profile Profile.Model
    | Objective Objective.Model
    | ObjectiveList ObjectiveList.Model
    | Question Question.Model
    | Report Report.Model
