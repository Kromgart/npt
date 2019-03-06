module View.WorkOrders exposing (..)

-- Service modules

import Html exposing (Html, div, a, span, text, table, thead, tfoot, tbody, tr, td, th, br, strong, input, label, ul, li, button)
import Html.Attributes exposing (id, class, classList, for, colspan, rowspan, style, value, type_, disabled, checked, placeholder, href)
import Html.Events exposing (on, onWithOptions, onFocus, onInput, onBlur, onCheck, onClick)
import Maybe.Extra
import Date exposing (Date)
import Date.Format exposing (format)
import FontAwesome.Web as Icon
import Json.Decode as Decode
import DOM
import Debug exposing (log)


-- Application modules

import Page.WorkOrders as WOs exposing (Model, NewWOState, Msg(..), setApprove, setBudget, setExtBudget, setNonBudget, extractWO)
import Data.WorkOrders exposing (WORowID, WORow, EWORow)
import Data.Shared exposing (DataStorage, ProjectID, Project, ProjectTypeID, ProjectType, ResourceID, Resource, DataID(..), EditableData(..))
import Utils exposing (..)
import Module.SelectWithSearch as SWS
import Module.Collapsible as CLPS
import Module.RadialChart as RC


-- page view


view : Model -> DataStorage -> Html Msg
view model dataStorage =
    let
        pridList =
            List.foldl
                (\ewo acc ->
                    case extractWO ewo of
                        Just wo ->
                            wo.projectId :: acc

                        Nothing ->
                            acc
                )
                []
                model.woFeed

        provideNewWO renderer =
            model.newWOState
                |> Maybe.andThen filterNewEWORow
                >> Maybe.andThen (renderer dataStorage)
                >> Maybe.withDefault nothing
                >> newSection model

        provideOldWO renderer =
            dataStorage.projectTypes
                |> List.sortBy (\( _, rec ) -> rec.displayName)
                >> List.map
                    (section model
                        dataStorage
                        (List.map filterOldEWORow
                            >> Maybe.Extra.values
                            >> List.map (renderer dataStorage)
                            >> Maybe.Extra.values
                        )
                    )
    in
        div [ class "page-holder workorders" ]
            [ actionsPanel model
            , div
                [ class "workorders-table-holder scrolling-table-holder"
                , onClick <| SWS SWS.OnClose
                ]
                [ header model
                , footer model.woFeed
                , div [ class "scrolling-table-body" ]
                    (backgroundTable
                        :: (newBodyRow |> provideNewWO)
                        :: (bodyRow |> provideOldWO)
                    )
                , div [ class "scrolling-table-sidebar" ]
                    ((newSidebarRow |> provideNewWO)
                        :: (sidebarRow |> provideOldWO)
                    )
                ]
            , SWS.view dataStorage.projects pridList model.selectProject
                |> Html.map SWS
            ]


header : Model -> Html Msg
header model =
    div [ class "scrolling-table-header" ]
        [ table [ class "workorders-table scrolling-table" ]
            [ thead []
                [ tr []
                    [ th [ class "project-name-cell" ] [ text "Project Name", Html.map CLPS <| CLPS.filter model.collapsible ]
                    , th [ class "main-cell" ] [ text "Budget" ]
                    , th [ class "main-cell" ] [ text "Extra Budget" ]
                    , th [] [ text "Not Billable" ]
                    , th [] [ text "Total Budget" ]
                    , th [] [ text "Capacity Booked" ]
                    , th [] [ text "Account Manager" ]
                    , th [] [ text "Project Manager" ]
                    , th [] [ text "Approved WO" ]
                    , th [ class "actions-cell" ] []
                    ]
                ]
            ]
        ]


footer : List EWORow -> Html Msg
footer woList =
    div [ class "scrolling-table-footer" ]
        [ table [ class "workorders-table scrolling-table" ]
            [ tfoot []
                [ summaryRow (text "Hours Total") woList
                ]
            ]
        ]


summaryRow : Html Msg -> List EWORow -> Html Msg
summaryRow title woList =
    let
        calculateTotal : (WORow -> Int) -> List EWORow -> Int
        calculateTotal getter =
            Maybe.withDefault 0
                << List.foldl
                    (\wo acc ->
                        Maybe.map2 (\wo acc -> getter wo + acc) (extractWO wo) acc
                    )
                    (Just 0)

        budget =
            calculateTotal .budget woList

        extraBudget =
            calculateTotal .extraBudget woList

        nonBillable =
            calculateTotal .nonBillable woList

        bookedTotal =
            calculateTotal .bookedTotal woList

        sum =
            budget + extraBudget + nonBillable
    in
        tr []
            [ th [ class "project-name-cell" ] [ title ]
            , td [ class "main-cell" ] [ text <| toHoursValue budget ++ " " ++ Tuple.first timeUnits ]
            , td [ class "main-cell" ] [ text <| toHoursValue extraBudget ++ " " ++ Tuple.first timeUnits ]
            , td [] [ text <| toHoursValue nonBillable ++ " " ++ Tuple.first timeUnits ]
            , td [] [ text <| toHoursValue sum ++ " " ++ Tuple.first timeUnits ]
            , td [] [ text <| toHoursValue bookedTotal ++ " " ++ Tuple.first timeUnits ]
            , td [] []
            , td [] []
            , td [] []
            , td [ class "actions-cell" ] []
            ]


backgroundTable : Html Msg
backgroundTable =
    table [ class "workorders-table scrolling-table empty-space" ]
        [ tbody []
            [ tr []
                [ th [ class "project-name-cell" ] []
                , td [ class "main-cell" ] []
                , td [ class "main-cell" ] []
                , td [] []
                , td [ class "read-only-cell" ] []
                , td [ class "read-only-cell" ] []
                , td [] []
                , td [] []
                , td [] []
                , td [ class "actions-cell" ] []
                ]
            ]
        ]


actionsPanel : Model -> Html Msg
actionsPanel model =
    div [ class "actions-panel" ]
        [ button [ class "btn-new-workorder", onClick EWORowCreate ] [ Icon.plus, text " ADD NEW WORK ORDER" ]
        , button [ class "prev-month", onClick SubstractMonth ] [ Html.i [ class "fa fa-chevron-left" ] [] ]
        , div [ class "fake-report" ]
            [ input [ id "reportrange" ] []
            , span [ class "label" ] [ text "Month: " ]
            , strong [] [ text <| format "%B " model.dateRange.startDate ]
            , text <| format "%Y" model.dateRange.startDate
            , text " "
            , Icon.calendar
            ]
        , button [ class "next-month", onClick AddMonth ] [ Html.i [ class "fa fa-chevron-right" ] [] ]
        ]


filterNewEWORow : EWORow -> Maybe EWORow
filterNewEWORow eWORow =
    case eWORow of
        New _ ->
            Just eWORow

        Creating _ ->
            Just eWORow

        _ ->
            Nothing


filterOldEWORow : EWORow -> Maybe EWORow
filterOldEWORow eWORow =
    case eWORow of
        Actual woId wo cachedWo ->
            Just eWORow

        Updating woId wo cachedWo ->
            Just eWORow

        Deleting woId wo cachedWo ->
            Just eWORow

        Error message (Actual woId wo cachedWo) ->
            Just eWORow

        _ ->
            Nothing


newSection : Model -> Html Msg -> Html Msg
newSection model content =
    table [ class "workorders-table scrolling-table" ]
        [ tbody []
            [ content
            ]
        ]


section : Model -> DataStorage -> (List EWORow -> List (Html Msg)) -> ( ProjectTypeID, ProjectType ) -> Html Msg
section model dataStorage contentList ( (DataID id) as projectTypeID, projectType ) =
    let
        filteredFeed =
            model.woFeed
                |> List.filter (funcWithDefault False (\( _, pr ) -> pr.projectType == projectTypeID))
                >> List.sortBy (funcWithDefault "" (\( _, pr ) -> pr.displayName))

        funcWithDefault d f =
            extractWO
                >> Maybe.andThen (\wo -> Maybe.map f <| findRecById wo.projectId dataStorage.projects)
                >> Maybe.withDefault d
    in
        if List.length filteredFeed > 0 then
            CLPS.section CLPS
                id
                model.collapsible
                (span [] [ text projectType.displayName ])
                (table [ class "workorders-table scrolling-table" ]
                    [ tbody []
                        (contentList filteredFeed)
                    , tfoot [] [ summaryRow (text "Hours SubTotal") filteredFeed ]
                    ]
                )
        else
            nothing


rowHeader : List ( ProjectID, Project ) -> WORow -> List (Html Msg)
rowHeader projects wo =
    let
        project =
            findRecById wo.projectId projects

        ( projectName, projectColor ) =
            Maybe.withDefault ( "", "" ) <| Maybe.map (\( id, pr ) -> ( pr.displayName, pr.color )) project

        efficiency =
            if (wo.bookedTotal == 0) then
                Nothing
            else
                Just <| toFloat (round (toFloat wo.loggedTotal / toFloat wo.bookedCurrent * 100)) / 100

        effMarker =
            Maybe.andThen
                (\efficiency ->
                    if efficiency < 0.65 || efficiency > 1.35 then
                        Just "#ff0000"
                    else if efficiency < 0.85 || efficiency > 1.15 then
                        Just "#ffc600"
                    else
                        Nothing
                )
                efficiency

        diff =
            wo.loggedTotal - wo.bookedCurrent

        message =
            toHoursValue (abs diff) ++ " " ++ Tuple.first timeUnits ++ " " ++ (diff < 0 ? "Behind" <| "Ahead") ++ " Schedule"
    in
        [ div [ class "efficiency-chart" ]
            (RC.radialChart efficiency projectColor
                :: Maybe.Extra.values
                    [ Maybe.map RC.indicator effMarker
                    , Maybe.map (\_ -> RC.tooltip message) effMarker
                    ]
            )
        , text
            (if String.isEmpty projectName then
                "Not Selected"
             else
                projectName
            )
        ]


sidebarRow : DataStorage -> EWORow -> Maybe (Html Msg)
sidebarRow { projects } eWORow =
    Maybe.map (\wo -> tr [] [ th [ class "project-name-cell" ] (rowHeader projects wo) ]) <| extractWO eWORow


bodyRow : DataStorage -> EWORow -> Maybe (Html Msg)
bodyRow { projects, resources } eWORow =
    Maybe.map
        (\wo ->
            let
                project =
                    findRecById wo.projectId projects
            in
                tr []
                    (List.concat
                        [ [ th [ class "project-name-cell" ] (rowHeader projects wo)
                          , td [ class "text-field-cell main-cell" ]
                                [ input [ type_ "text", onInput <| EWORowInput setBudget eWORow, onBlur <| EWORowUpdate eWORow, value <| toHoursValue wo.budget ] []
                                , span [ class "fake" ] [ text <| toHoursValue wo.budget ++ " " ++ Tuple.first timeUnits ]
                                , span [ class "units" ] [ text <| Tuple.first timeUnits ]
                                ]
                          , td [ class "text-field-cell main-cell" ]
                                [ input [ type_ "text", onInput <| EWORowInput setExtBudget eWORow, onBlur <| EWORowUpdate eWORow, value <| toHoursValue wo.extraBudget ] []
                                , span [ class "fake" ] [ text <| toHoursValue wo.extraBudget ++ " " ++ Tuple.first timeUnits ]
                                , span [ class "units" ] [ text <| Tuple.first timeUnits ]
                                ]
                          , td [ class "text-field-cell" ]
                                [ input [ type_ "text", onInput <| EWORowInput setNonBudget eWORow, onBlur <| EWORowUpdate eWORow, value <| toHoursValue wo.nonBillable ] []
                                , span [ class "fake" ] [ text <| toHoursValue wo.nonBillable ++ " " ++ Tuple.first timeUnits ]
                                , span [ class "units" ] [ text <| Tuple.first timeUnits ]
                                ]
                          ]
                        , readOnlyCells wo project resources
                        , [ td []
                                [ label [ class "pretty-checkbox" ]
                                    [ input [ type_ "checkbox", onCheck <| EWORowCheck setApprove eWORow, checked wo.isApproved ] []
                                    , div [] []
                                    ]
                                ]
                          , td [ class "actions-cell" ] [ button [ class "btn", onClick <| EWORowDelete <| eWORow ] [ Icon.trash ] ]
                          ]
                        ]
                    )
        )
    <|
        extractWO eWORow


newSidebarRow : DataStorage -> EWORow -> Maybe (Html Msg)
newSidebarRow { projects } eWORow =
    Maybe.map
        (\wo ->
            tr [ class "workorder-new-row" ]
                [ th [ class "project-name-cell" ]
                    ((Html.i [ class "fa fa-list-ul" ] []
                        |> SWS.btn
                        |> Html.map SWS
                     )
                        :: (rowHeader projects wo)
                    )
                ]
        )
    <|
        extractWO eWORow


newBodyRow : DataStorage -> EWORow -> Maybe (Html Msg)
newBodyRow { projects, resources } eWORow =
    Maybe.map
        (\wo ->
            let
                project =
                    findRecById wo.projectId projects

                projectName =
                    Maybe.withDefault "" <| Maybe.map (\( id, pr ) -> pr.displayName) project

                disabled =
                    String.isEmpty projectName || wo.budget == 0
            in
                tr [ class "workorder-new-row" ]
                    (List.concat
                        [ [ th [ class "project-name-cell" ] (rowHeader projects wo)
                          , td [ class "text-field-cell main-cell" ]
                                [ input [ type_ "text", onInput <| EWORowInput setBudget eWORow, value <| toHoursValue wo.budget ] []
                                , span [ class "fake" ] [ text <| toHoursValue wo.budget ++ " " ++ Tuple.first timeUnits ]
                                , span [ class "units" ] [ text <| Tuple.first timeUnits ]
                                ]
                          , td [ class "text-field-cell main-cell" ]
                                [ input [ type_ "text", onInput <| EWORowInput setExtBudget eWORow, value <| toHoursValue wo.extraBudget ] []
                                , span [ class "fake" ] [ text <| toHoursValue wo.extraBudget ++ " " ++ Tuple.first timeUnits ]
                                , span [ class "units" ] [ text <| Tuple.first timeUnits ]
                                ]
                          , td [ class "text-field-cell" ]
                                [ input [ type_ "text", onInput <| EWORowInput setNonBudget eWORow, value <| toHoursValue wo.nonBillable ] []
                                , span [ class "fake" ] [ text <| toHoursValue wo.nonBillable ++ " " ++ Tuple.first timeUnits ]
                                , span [ class "units" ] [ text <| Tuple.first timeUnits ]
                                ]
                          ]
                        , readOnlyCells wo project resources
                        , [ td []
                                [ label [ class "pretty-checkbox" ]
                                    [ input [ type_ "checkbox", onCheck <| EWORowCheck setApprove eWORow, checked wo.isApproved ] []
                                    , div [] []
                                    ]
                                ]
                          , td [ class "actions-cell" ]
                                [ button
                                    (List.append
                                        [ classList [ ( "btn", True ), ( "disabled", disabled ) ] ]
                                        (if disabled then
                                            [ Html.Attributes.disabled disabled ]
                                         else
                                            [ onClick <| EWORowUpdate <| eWORow ]
                                        )
                                    )
                                    [ Icon.check ]
                                , button [ class "btn", onClick <| EWORowCancel ] [ Icon.times ]
                                ]
                          ]
                        ]
                    )
        )
    <|
        extractWO eWORow


readOnlyCells : WORow -> Maybe ( ProjectID, Project ) -> List ( ResourceID, Resource ) -> List (Html Msg)
readOnlyCells wo project resources =
    [ td [ class "read-only-cell" ] [ text <| toHoursValue (wo.extraBudget + wo.nonBillable + wo.budget) ++ Tuple.first timeUnits ]
    , td [ class "read-only-cell" ] [ text <| toHoursValue wo.bookedTotal ++ Tuple.first timeUnits ]
    , td []
        [ div [ class "person-photo" ] []
        , text <| getResName (project |> Maybe.andThen (\( id, pr ) -> pr.accountMgr)) resources
        ]
    , td []
        [ div [ class "person-photo" ] []
        , text <| getResName (project |> Maybe.andThen (\( id, pr ) -> pr.projectMgr)) resources
        ]
    ]


getResName : Maybe ResourceID -> List ( ResourceID, Resource ) -> String
getResName resId resources =
    resId
        |> Maybe.andThen (\amId -> findRecById amId resources)
        |> Maybe.map (\( id, res ) -> res.name)
        |> Maybe.withDefault "Not assigned"
