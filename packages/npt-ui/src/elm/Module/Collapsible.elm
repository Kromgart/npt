module Module.Collapsible exposing (..)

-- service modules

import Html exposing (Html, text, div, span, input, table, tbody, button, ul, li, label)
import Html.Attributes as Attr exposing (id, type_, classList, class, style, checked, for)
import Html.Events exposing (onClick, onInput, onWithOptions, onCheck)
import Set exposing (Set)
import List.Extra
import Task
import DOM
import Json.Decode as Decode
import FontAwesome.Web as Icon


-- application modules

import Module.Collapsible.Ports as Ports
import Data.Shared exposing (DataID(..))
import Utils exposing (..)


-- Module model


type alias Model =
    { collapsed : Set Int
    , all : List ( DataID Int, String )
    }


init : Set Int -> List ( DataID Int, String ) -> Model
init collapsed all =
    Model collapsed all



-- Module messages


type Msg
    = Collapse Int
    | Expand Int
    | Toggle Int Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Collapse cid ->
            ( { model | collapsed = Set.insert cid model.collapsed }, Ports.collapsibleCollapse cid )

        Expand cid ->
            ( { model | collapsed = Set.remove cid model.collapsed }, Ports.collapsibleExpand cid )

        Toggle cid state ->
            ( model
            , Task.perform
                (if state then
                    Expand
                 else
                    Collapse
                )
              <|
                Task.succeed cid
            )


section : (Msg -> a) -> Int -> Model -> Html a -> Html a -> Html a
section f cid model buttonInner content =
    let
        isCollased =
            Set.member cid model.collapsed
    in
        div [ Attr.attribute "data-cid" <| "cid" ++ toString cid, classList [ ( "collapsible-section", True ), ( "collapsed", isCollased ) ] ]
            [ button
                [ class "opener"
                , Attr.map f <|
                    onClick <|
                        if isCollased then
                            Expand cid
                        else
                            Collapse cid
                ]
                [ buttonInner ]
            , div [ class "collapsible-content" ] [ content ]
            ]


filter : Model -> Html Msg
filter model =
    let
        drawCheckbox ( DataID cid, name ) =
            li []
                [ input
                    [ type_ "checkbox"
                    , id <| "cid" ++ toString cid
                    , onCheck <| Toggle cid
                    , checked <| not <| Set.member cid model.collapsed
                    ]
                    []
                , label [ for <| "cid" ++ toString cid ] [ text name ]
                ]
    in
        div [ class "content-filter" ]
            [ span [ class "content-filter-icon" ] [ Icon.eye ]
            , div [ class "content-filter-dropdown" ]
                [ text "Show"
                , ul [] (List.map drawCheckbox <| List.sortBy (\( _, rec ) -> rec) model.all)
                ]
            ]
