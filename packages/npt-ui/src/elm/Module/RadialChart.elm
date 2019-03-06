module Module.RadialChart exposing (..)

-- Service modules

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)


-- Application modules

import Utils exposing (..)


radialChart : Maybe Float -> String -> Html msg
radialChart prct color =
    let
        ( deg, message ) =
            case prct of
                Just prct ->
                    ( (prct > 1 ? 1 <| (prct < 0 ? 0 <| prct)) * 180, toString (prct * 100) ++ Tuple.first prctUnits )

                Nothing ->
                    ( 0, "--" )
    in
        div [ class "radial-progress", style [ ( "background-color", color ) ] ]
            [ div [ class "circle" ]
                [ div [ class "mask left" ]
                    [ div [ class "fill", style [ ( "transform", "rotate(" ++ toString deg ++ "deg)" ) ] ]
                        []
                    , div [ class "fill fix", style [ ( "transform", "rotate(" ++ toString (deg * 2) ++ "deg)" ) ] ]
                        []
                    ]
                , div [ class "mask right", style [ ( "transform", "rotate(" ++ toString (deg - 180) ++ "deg)" ) ] ]
                    [ div [ class "fill", style [ ( "transform", "rotate(" ++ toString deg ++ "deg)" ) ] ]
                        []
                    ]
                ]
            , div [ class "info", style [ ( "background-color", color ) ] ]
                [ text message
                ]
            ]


tooltip : String -> Html msg
tooltip message =
    div [ class "tooltip" ] [ text message ]


indicator : String -> Html msg
indicator color =
    div [ class "indicator", style [ ( "background-color", color ) ] ] []
