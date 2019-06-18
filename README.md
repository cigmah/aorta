# AORTA: An Open Revision Tool for Assessments

This repo contains the frontend code for AORTA - an open revision tool for assessments.

AORTA is a project of [CIGMAH, the Coding Interest Group in Medicine and Healthcare](https://cigmah.github.io/).

## Structure

The structure of AORTA's single-page-app is based on the structure of
`rtfeldman`'s [`elm-spa-example`](https://github.com/rtfeldman/elm-spa-example).

Here's a quick rundown:

1. The app entry-point is `App.elm` in the `src` directory.
2. The `Types/` directory contains types and functions relating to those types, like the `Question` type for EMQ questions.
3. The `Page/` directory contains pages with their own little Elm architecture layout and are mostly self-contained.
   In order to switch pages, each page also has an `eject` function which extracts the data that's meant to persist
   between pages, like the active page or username.
4. The `Architecture/` directory contains most of the boilerplate needed to glue all the
   pages together into one SPA. It's mostly a lot of boilerplate.

## Adding a Page

Adding a page requires some boilerplate.

1. Copy `template.elm` into a new file in the `Page/` directory.
2. Add the route to `Route.elm` as part of the `Route` union type and add a path to the parser.
3. Import the page and add a model mapper to `Model.elm` as part of the `Model` union type.
4. Import the page and add a message mapper to `Msg.elm` as part of the `Msg` union type.
5. Import the page and add the `init` function to `Init.elm` under the `fromRoute` function.
6. Import the page and add the `subscribe` function to `Subscribe.elm` under the `subscriptions` function.
7. Import the page and add the relevant functions to `Update.elm` under the `update`, `eject` and `reroute` functions.
8. Import the page and add the `view` function to `View.elm` under the `view` function.

It's a lot, yes. This should be scriptable, so we may make a script for it sometime.
