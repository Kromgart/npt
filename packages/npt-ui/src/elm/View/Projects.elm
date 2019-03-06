module View.Projects exposing (..)

-- external imports

import Html exposing (Html, div, text)


-- internal imports

import Page.Projects exposing (Model)


-- page view


view : Model -> Html msg
view model =
    text "Projects"
