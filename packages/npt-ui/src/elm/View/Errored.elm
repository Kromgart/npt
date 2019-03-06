module View.Errored exposing (..)

import Html exposing (Html, div, text)
import Page.Errored exposing (Model)


view : Model -> Html msg
view model =
    div []
        [ text <| toString model
        ]
