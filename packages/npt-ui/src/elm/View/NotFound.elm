module View.NotFound exposing (..)

-- external imports

import Html exposing (Html, div, text)


-- page view


view : Html msg
view =
    div [] [ text "Sorry! Page is missing. Try another one." ]
