module Module.SelectWithSearch exposing (..)

-- service modules

import Html exposing (Html, text, div, span, input, ul, li)
import Html.Attributes exposing (type_, class, style, placeholder)
import Html.Events exposing (onClick, onInput, onWithOptions)
import List.Extra
import Task
import DOM
import Json.Decode as Decode


-- application modules

import Data.Shared exposing (DataID(..), ProjectID, Project)
import Utils exposing (..)


-- Module model


type alias Model =
    { id : Maybe ProjectID
    , isOpened : Bool
    , filter : String
    , rect : DOM.Rectangle
    }



-- Module messages


type Msg
    = OnClick DOM.Rectangle
    | OnClose
    | OnFilterInput String
    | OnSelect ProjectID
    | Selected ProjectID



-- Init model with specific item id


init : Maybe ProjectID -> Model
init id =
    Model id False "" <| DOM.Rectangle 0 0 0 0



-- module update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Toggling visibility on click
        OnClick rect ->
            ( { model | rect = rect, isOpened = not model.isOpened }, Cmd.none )

        OnClose ->
            ( { model | rect = DOM.Rectangle 0 0 0 0, isOpened = False }, Cmd.none )

        -- Set filter string
        OnFilterInput value ->
            ( { model | filter = value }, Cmd.none )

        -- Select clicked item and fire Selected message
        OnSelect prid ->
            ( init <| Just prid, Task.perform Selected <| Task.succeed prid )

        -- Skip Selected message. You should handle it in your update function
        _ ->
            ( model, Cmd.none )



-- button view


btn : Html Msg -> Html Msg
btn icon =
    div [ class "select-project-holder" ]
        [ span
            [ class "select-project-btn"
            , onWithOptions "click"
                { stopPropagation = True, preventDefault = False }
                (Decode.map OnClick <| DOM.target DOM.boundingClientRect)
            ]
            [ icon ]
        ]



-- dropwdown view


view : List ( ProjectID, Project ) -> List ProjectID -> Model -> Html Msg
view projects idListToFilter { id, filter, isOpened, rect } =
    case isOpened of
        True ->
            div
                [ class "select-with-serach-dropdown"
                , style [ ( "top", toString rect.top ++ "px" ), ( "left", toString rect.left ++ "px" ) ]
                ]
                [ span [ class "title" ] [ text "Assign New Project" ]
                , input [ type_ "search", placeholder "Search", onInput OnFilterInput ] []
                , ul []
                    (List.map
                        (\( prid, project ) ->
                            let
                                intersect =
                                    case Maybe.map ((==) prid) id of
                                        Just True ->
                                            True

                                        _ ->
                                            False
                            in
                                case intersect of
                                    False ->
                                        if String.contains (String.toLower filter) (String.toLower project.displayName) then
                                            li
                                                [ style [ ( "background-color", project.color ) ]
                                                , onClick (OnSelect prid)
                                                ]
                                                [ text project.displayName ]
                                        else
                                            nothing

                                    True ->
                                        nothing
                        )
                        (List.filter (\( prid, project ) -> List.Extra.notMember prid idListToFilter) projects)
                    )
                ]

        False ->
            nothing
