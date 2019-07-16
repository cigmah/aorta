# AORTA: An Open Revision Tool for Assessments

This repo contains the frontend code for AORTA - an open revision tool for assessments.

AORTA is a project of [CIGMAH, the Coding Interest Group in Medicine and Healthcare](https://cigmah.github.io/).

We are currently hosting a frontend at https://aorta.netlify.com.

# Developing Locally

Clone this repository.

```
git clone https://github.com/cigmah/aorta.git
```

To develop locally, you'll need to have [Elm](https://elm-lang.org/) and [create-elm-app](https://github.com/halfzebra/create-elm-app) installed (and therefore `npm` as well). You'll also need [tailwindcss](https://tailwindcss.com/) to build the styles.

If you're setting things up locally, you'll probably want to have the backend setup locally as well. See the [aorticroot](https://github.com/cigmah/aorticroot) repo for instructions on setting up the backend locally. For local development, to make sure both the frontend and backend can communicate, you may need to modify the `CORS_ORIGIN_WHITELIST`(or set `CORS_ORIGIN_ALLOW_ALL = True`). 

You'll also need to create a file, `src/Secret.elm` (though it's not holding anything secret) which will hold the URL for the backend API, e.g. if you're testing the backend on `127.0.0.1:8000`: 

```
module Secret exposing (apiBase)

apiBase : String
apiBase =
    "http://127.0.0.1:8000"
```

Finally, make sure to build the first styles:

```sh
npx tailwind build src/styles.css -o src/output.css
```

You should now be able to run the app from this directory with:

```
elm-app start
```

To build, run:

```
elm-app build
```

If building for Netlify, then also copy the `netlify.toml` file into the build directory.

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
2. Add the route to `Route.elm` as part of the `Route` union type and add a string form to the `toString` function.
3. Import the page and add a model mapper to `Model.elm` as part of the `Model` union type.
4. Import the page and add a message mapper to `Msg.elm` as part of the `Msg` union type.
5. Import the page and add the `init` function to `Init.elm` under the `fromRoute` function.
6. Import the page and add the `subscribe` function to `Subscribe.elm` under the `subscriptions` function.
7. Import the page and add the relevant functions to `Update.elm` under the `update`, `eject` and `inject` functions.
8. Import the page and add the `view` function to `View.elm` under the `view` function.

It's a lot, yes. This should be scriptable, so we may make a script for it sometime.

## Styles

This project uses Tailwind CSS for styling.

To build the styles from `src/styles.css`, run:

```sh
npx tailwind build src/styles.css -o src/output.css
```

from this directory.

## Development TODOs

- [ ] Refactor all HTML elements out of pages and into either `Page/Elements.elm`
  or a different type module. Currently in progress.
- [ ] Simplify update functions for each page - a lot are unwieldy at the moment.
- [ ] Pagination for response history.
- [ ] Follow up backend roadmap endpoints as they are added.
