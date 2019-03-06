module View.Resources exposing (..)

-- Service modules

import Html exposing (Html, div, span, text, table, thead, tbody, tr, td, th, br, strong, input, label, ul, li)
import Html.Attributes exposing (id, class, classList, for, colspan, rowspan, style, value, type_, disabled, checked, placeholder)
import Html.Events exposing (on, onWithOptions, onFocus, onInput, onBlur, onCheck, onClick)
import Date exposing (Date)
import Date.Format exposing (format)
import Maybe.Extra
import Set exposing (Set)
import List.Extra
import FontAwesome.Web as Icon
import Json.Decode as Decode
import DOM


-- Application modules

import Page.Resources exposing (Model, RowFilterState, Msg(..), AssignDropdown)
import Data.Shared exposing (DataStorage, EditableData(..), DataID(..), Resource, ResourceID, Project, ProjectID)
import Data.Resources exposing (PlanResRow, PlanResProjectRow, EBooking, Booking, PlanResCapacity(..))
import Utils exposing (..)
import Debug exposing (log)


-- page content


view : Model -> DataStorage -> Html Msg
view model dataStorage =
    let
        planningTable =
            model.planningFeed

        filterState =
            model.rowFilterState

        dates =
            createDateList model.dateRange.startDate model.dateRange.endDate
    in
        div [ class "page-holder resources" ]
            [ actionsPanel model
            , div
                [ class "people-table-holder scrolling-table-holder"
                , on "click" (Decode.map (ShowProjectsList <| Nothing) (DOM.target DOM.boundingClientRect))
                ]
                [ div [ class "scrolling-table-header" ]
                    [ table [ class "people-table scrolling-table" ]
                        [ thead []
                            (List.append
                                [ th [ class "people-person-name-cell" ]
                                    [ text "Person Name"
                                    , rowFilter filterState
                                    ]
                                , th [ class "people-project-name-cell" ]
                                    [ text "Project Name" ]
                                ]
                                (List.map
                                    (\date ->
                                        th [] [ text <| format "%a, %b %d" date ]
                                    )
                                    dates
                                )
                            )
                        ]
                    ]
                , div [ class "scrolling-table-body" ]
                    [ table [ class "people-table scrolling-table" ]
                        [ tbody []
                            (List.concatMap (personRow dataStorage model) planningTable)
                        ]
                    ]
                , div [ class "scrolling-table-sidebar" ]
                    [ table [ class "people-table scrolling-table" ]
                        [ tbody []
                            (List.concatMap (sidebarRow dataStorage model) planningTable)
                        ]
                    ]
                ]
            , addProjectDrowdown model dataStorage.projects model.assignDropdown
            ]


actionsPanel : Model -> Html Msg
actionsPanel model =
    div [ class "actions-panel" ]
        [ div [ id "reportrange" ]
            [ span [ class "label" ] [ text "From: " ]
            , text <| format "%B %d, %Y" model.dateRange.startDate
            , span [ class "label" ] [ text " To: " ]
            , text <| format "%B %d, %Y" model.dateRange.endDate
            , text " "
            , Icon.calendar
            ]
        ]


rowFilter : RowFilterState -> Html Msg
rowFilter filterState =
    let
        drawCheckboxRow name =
            if name /= "Available" then
                li []
                    [ input
                        [ type_ "checkbox"
                        , id name
                        , onCheck <| RowFilterSelected name
                        , checked <|
                            Set.member name filterState.selected
                        ]
                        []
                    , label [ for name ] [ text name ]
                    ]
            else
                nothing
    in
        div [ class "content-filter" ]
            [ span [ class "content-filter-icon" ] [ Icon.eye ]
            , div [ class "content-filter-dropdown" ]
                [ text "Show"
                , ul [] <| List.map drawCheckboxRow filterState.rows
                ]
            ]


addProjectDrowdown : Model -> List ( ProjectID, Project ) -> AssignDropdown -> Html Msg
addProjectDrowdown model projs { filter, currentResource, rect } =
    case currentResource of
        Just currentResource ->
            div
                [ class "select-with-serach-dropdown"
                , style [ ( "top", toString rect.top ++ "px" ), ( "left", toString rect.left ++ "px" ) ]
                ]
                [ span [ class "title" ] [ text "Assign New Project" ]
                , input [ type_ "search", placeholder "Search", onInput InputNewProjectFilter ] []
                , ul []
                    (List.map
                        (\( id, rec ) ->
                            case List.Extra.find (\project -> project.projectId == id) currentResource.booked of
                                Nothing ->
                                    if String.contains (String.toLower filter) (String.toLower rec.displayName) then
                                        li
                                            [ style [ ( "background-color", rec.color ) ]
                                            , onClick (AddPlanResProjectRow currentResource.resId id)
                                            ]
                                            [ text rec.displayName ]
                                    else
                                        nothing

                                _ ->
                                    nothing
                        )
                        projs
                    )
                ]

        Nothing ->
            nothing


personRow : DataStorage -> Model -> PlanResRow -> List (Html Msg)
personRow dataStorage ({ dateRange, rowFilterState } as model) planResource =
    let
        resource =
            findRecById planResource.resId dataStorage.resources

        project booked =
            findRecById booked.projectId dataStorage.projects

        capacities =
            List.map
                (\( date, capacity ) ->
                    case capacity of
                        Some amount ->
                            amount

                        _ ->
                            0
                )
                planResource.capacities

        maybeEmptyRow rsValue =
            Maybe.map (emptyRow rsValue <| List.length capacities) resource

        maybePlaceholderRow =
            Just <| placeholderRow <| List.length capacities

        maybeProjectRow booked =
            Maybe.map2 (projectRow (createDateList dateRange.startDate dateRange.endDate) booked) resource (project booked)

        maybeSummaryRow heading summary ( shortUnit, fullUnit ) items children =
            if (Set.member heading rowFilterState.selected) then
                Just (summaryRow heading summary ( shortUnit, fullUnit ) items children)
            else
                Nothing

        plannedDays =
            List.foldl
                (\project acc ->
                    List.map2
                        (\booking accAmount ->
                            case booking of
                                Actual bookingId bRec cachedRec ->
                                    cachedRec.amount + accAmount

                                Updating bookingId bRec cachedRec ->
                                    cachedRec.amount + accAmount

                                Deleting bookingId bRec cachedRec ->
                                    cachedRec.amount + accAmount

                                _ ->
                                    accAmount
                        )
                        project.bookings
                        acc
                )
                (List.repeat (List.length capacities) 0)
                planResource.booked

        capacitiesCalc =
            List.map (\capacity -> (toFloat capacity) / 60) capacities

        availables =
            List.map2 (\plannedDay capacity -> (toFloat (capacity - plannedDay)) / 60) plannedDays capacities

        efficiencies =
            List.map2 (\plannedDay capacity -> roundToFirstSign <| toFloat (plannedDay * 100) / toFloat capacity) plannedDays capacities

        rsValue =
            List.length planResource.booked + 2
    in
        Maybe.Extra.values <|
            List.concat
                [ if List.length planResource.booked > 0 then
                    maybeEmptyRow rsValue :: List.map maybeProjectRow planResource.booked
                  else
                    [ maybeEmptyRow (rsValue + 1), maybePlaceholderRow ]
                , [ maybeEmptyRow 0
                  , maybeSummaryRow "Capacity"
                        (List.sum capacitiesCalc)
                        timeUnits
                        capacitiesCalc
                        []
                  , maybeSummaryRow "Available"
                        (List.sum availables)
                        timeUnits
                        availables
                        []
                  , maybeSummaryRow "Efficiency"
                        (roundToFirstSign (List.sum efficiencies / (toFloat <| List.length efficiencies)))
                        prctUnits
                        efficiencies
                        []
                  ]
                ]


sidebarRow : DataStorage -> Model -> PlanResRow -> List (Html Msg)
sidebarRow dataStorage ({ dateRange, rowFilterState } as model) planResource =
    let
        resource =
            findRecById planResource.resId dataStorage.resources

        project booked =
            findRecById booked.projectId dataStorage.projects

        capacities =
            List.map
                (\( date, capacity ) ->
                    case capacity of
                        Some amount ->
                            amount

                        _ ->
                            0
                )
                planResource.capacities

        maybeEmptyRow rsValue =
            Maybe.map (emptyRowSidebar rsValue) resource

        maybePlaceholderRow =
            Just <| placeholderRowSidebar

        maybeProjectRow booked =
            Maybe.map projectRowSidebar (project booked)

        maybeSummaryRow heading summary ( shortUnit, fullUnit ) children =
            if (Set.member heading rowFilterState.selected) then
                Just (summaryRowSidebar heading summary ( shortUnit, fullUnit ) children)
            else
                Nothing

        plannedDays =
            List.foldl
                (\project acc ->
                    List.map2
                        (\booking accAmount ->
                            case booking of
                                Actual bookingId bRec cachedRec ->
                                    cachedRec.amount + accAmount

                                Updating bookingId bRec cachedRec ->
                                    cachedRec.amount + accAmount

                                Deleting bookingId bRec cachedRec ->
                                    cachedRec.amount + accAmount

                                _ ->
                                    accAmount
                        )
                        project.bookings
                        acc
                )
                (List.repeat (List.length capacities) 0)
                planResource.booked

        capacitiesCalc =
            List.map (\capacity -> (toFloat capacity) / 60) capacities

        availables =
            List.map2 (\plannedDay capacity -> (toFloat (capacity - plannedDay)) / 60) plannedDays capacities

        efficiencies =
            List.map2 (\plannedDay capacity -> roundToFirstSign <| toFloat (plannedDay * 100) / toFloat capacity) plannedDays capacities

        rsValue =
            List.length planResource.booked + 2

        timeUnits =
            ( "Hr", "Hours" )

        prctUnits =
            ( "%", "%" )
    in
        Maybe.Extra.values <|
            List.concat
                [ if List.length planResource.booked > 0 then
                    maybeEmptyRow rsValue :: List.map maybeProjectRow planResource.booked
                  else
                    [ maybeEmptyRow (rsValue + 1), maybePlaceholderRow ]
                , [ maybeEmptyRow 0
                  , maybeSummaryRow "Capacity"
                        (List.sum capacitiesCalc)
                        timeUnits
                        []
                  , maybeSummaryRow "Available"
                        (List.sum availables)
                        timeUnits
                        [ div [ class "people-add-new-project-holder" ]
                            [ span
                                [ class "people-add-new-project-button"
                                , onWithOptions "click"
                                    { stopPropagation = True, preventDefault = False }
                                    (Decode.map (ShowProjectsList <| Just planResource) (DOM.target DOM.boundingClientRect))
                                ]
                                [ Icon.plus_circle ]
                            ]
                        ]
                  , maybeSummaryRow "Efficiency"
                        (roundToFirstSign (List.sum efficiencies / (toFloat <| List.length efficiencies)))
                        prctUnits
                        []
                  ]
                ]


placeholderRow : Int -> Html msg
placeholderRow qty =
    tr
        [ class "people-project-placeholder-row" ]
        (List.append
            [ td [ class "people-project-name-cell" ] [ text "No Assignment" ]
            ]
            (List.repeat qty (td [] []))
        )


placeholderRowSidebar : Html msg
placeholderRowSidebar =
    tr
        [ class "people-project-placeholder-row" ]
        [ td [ class "people-project-name-cell" ] [ text "No Assignment" ]
        ]


emptyRow : Int -> Int -> ( ResourceID, Resource ) -> Html msg
emptyRow rsValue qty ( _, resource ) =
    tr
        [ classList [ ( "people-person-start-row", rsValue > 0 ), ( "people-empty-row", rsValue == 0 ) ] ]
        (List.append
            [ personNameCell rsValue resource.name resource.title
            , td [ class "people-project-name-cell" ] []
            ]
            (List.repeat qty (td [] []))
        )


emptyRowSidebar : Int -> ( ResourceID, Resource ) -> Html msg
emptyRowSidebar rsValue ( _, resource ) =
    tr
        [ classList [ ( "people-person-start-row", rsValue > 0 ), ( "people-empty-row", rsValue == 0 ) ] ]
        [ personNameCell rsValue resource.name resource.title
        , td [ class "people-project-name-cell" ] []
        ]


summaryRow : String -> Float -> ( String, String ) -> List Float -> List (Html Msg) -> Html Msg
summaryRow heading summary ( shortUnit, fullUnit ) items children =
    tr
        [ class "people-summary-row" ]
        (List.append
            [ th [ class "people-person-name-cell" ]
                [ span [ class "people-text" ] [ text <| "Total " ++ String.toLower heading ++ ": " ]
                , span [ class "people-value" ]
                    [ strong [] [ text <| toString summary ]
                    , text <| " " ++ shortUnit
                    ]
                ]
            , td [ class "people-project-name-cell" ] <|
                span [] [ text heading ]
                    :: children
            ]
            (List.map
                (\item ->
                    td []
                        [ span [] [ text <| toString item ]
                        , text <| " " ++ shortUnit
                        ]
                )
                items
            )
        )


summaryRowSidebar : String -> Float -> ( String, String ) -> List (Html Msg) -> Html Msg
summaryRowSidebar heading summary ( shortUnit, _ ) children =
    tr
        [ class "people-summary-row" ]
        [ th [ class "people-person-name-cell" ]
            [ span [ class "people-text" ] [ text <| "Total " ++ String.toLower heading ++ ": " ]
            , span [ class "people-value" ]
                [ strong [] [ text <| toString summary ]
                , text <| " " ++ shortUnit
                ]
            ]
        , td [ class "people-project-name-cell" ] <|
            span [] [ text heading ]
                :: children
        ]


projectRow : List Date -> PlanResProjectRow -> ( ResourceID, Resource ) -> ( ProjectID, Project ) -> Html Msg
projectRow dates planProject ( resourceId, resource ) ( projectId, project ) =
    tr [ class "people-project-row" ]
        (td
            [ class "people-project-name-cell"
            , style [ ( "background-color", project.color ) ]
            ]
            [ text project.displayName ]
            :: List.map2
                (\date planBooking ->
                    let
                        ( val, inputState ) =
                            setInputState planBooking
                    in
                        td
                            [ class "people-project-booking-cell text-field-cell"
                            , style [ ( "background-color", project.color ) ]
                            ]
                            [ input
                                (List.append
                                    [ type_ "text"
                                    , Html.Attributes.max "24"
                                    , Html.Attributes.min "0"
                                    , Html.Attributes.step "0.5"
                                    , value val
                                    ]
                                    inputState
                                )
                                []
                            , span [ class "fake" ] [ text <| val ++ " " ++ Tuple.first timeUnits ]
                            , span [ class "units" ] [ text <| Tuple.first timeUnits ]
                            ]
                )
                dates
                planProject.bookings
        )


projectRowSidebar : ( ProjectID, Project ) -> Html Msg
projectRowSidebar ( _, project ) =
    tr
        [ class "people-project-row" ]
        [ td
            [ class "people-project-name-cell"
            , style [ ( "background-color", project.color ) ]
            ]
            [ text project.displayName ]
        ]


setInputState : EBooking -> ( String, List (Html.Attribute Msg) )
setInputState booking =
    case booking of
        Empty path ->
            ( toHoursValue 0
            , [ onFocus <| BookingCreate booking ]
            )

        New bRec ->
            ( toHoursValue bRec.amount
            , [ onInput <| BookingInput booking
              , onBlur <| BookingUpdate booking
              ]
            )

        Creating bRec ->
            ( toHoursValue bRec.amount
            , [ disabled True
              ]
            )

        Actual bookingId bRec cachedRec ->
            ( toHoursValue bRec.amount
            , [ onInput <| BookingInput booking
              , onBlur <| BookingUpdate booking
              ]
            )

        Updating bookingId bRec cachedRec ->
            ( toHoursValue bRec.amount
            , [ disabled True ]
            )

        Deleting bookingId bRec cachedRec ->
            ( toHoursValue bRec.amount
            , [ disabled True ]
            )

        Error message (New bRec) ->
            ( toHoursValue bRec.amount
            , [ onInput <| BookingInput booking
              , onBlur <| BookingUpdate booking
              ]
            )

        Error message (Actual bookingId bRec cachedRec) ->
            ( toHoursValue bRec.amount
            , [ onInput <| BookingInput booking
              , onBlur <| BookingUpdate booking
              ]
            )

        Error message (Empty path) ->
            ( toHoursValue 0
            , [ onFocus <| BookingCreate booking ]
            )

        Error message _ ->
            ( message
            , []
            )


personNameCell : Int -> String -> String -> Html msg
personNameCell rsValue name title =
    if (rsValue > 0) then
        th [ rowspan rsValue, class "people-person-name-cell" ]
            [ div [ class "people-person-photo" ] []
            , strong [] [ text name ]
            , br [] []
            , span [ class "people-person-post" ] [ text title ]
            ]
    else
        nothing
