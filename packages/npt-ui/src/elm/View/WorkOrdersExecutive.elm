module View.WorkOrdersExecutive exposing (..)

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
import List.Extra


-- Application modules

import Page.WorkOrdersExecutive exposing (Model, Msg(..), requestedGroups)
import Data.WorkOrdersExecutive exposing (WOExec)
import Data.Shared exposing (DataStorage, ProjectID, Project, ProjectTypeID, ProjectType, ResourceID, Resource, GroupID, Group, DataID(..), EditableData(..))
import Utils exposing (..)
import Module.SelectWithSearch as SWS
import Module.Collapsible as CLPS
import Module.RadialChart as RC


-- page view


view : Model -> DataStorage -> Html Msg
view model dataStorage =
    let
        pridList =
            List.map .projectId model.woexecFeed

        provideWO renderer =
            dataStorage.projectTypes
                |> List.sortBy (\( _, rec ) -> rec.displayName)
                >> List.map (section model dataStorage <| List.map renderer)
    in
        div [ class "page-holder workorders-executive" ]
            [ actionsPanel model
            , div
                [ class "workorders-table-holder executive scrolling-table-holder" ]
                [ header model dataStorage
                , footer model.woexecFeed dataStorage
                , div [ class "scrolling-table-body" ]
                    (backgroundTable dataStorage.groups :: (workOrderRow dataStorage |> provideWO))
                , div [ class "scrolling-table-sidebar" ]
                    (tr [] << List.singleton << rowHeader dataStorage.projects |> provideWO)
                ]
            ]


header : Model -> DataStorage -> Html Msg
header model dataStorage =
    div [ class "scrolling-table-header" ]
        [ table [ class "workorders-table scrolling-table" ]
            [ thead []
                [ tr []
                    (List.append
                        [ th [ class "project-name-cell" ] [ text "Project Name", Html.map CLPS <| CLPS.filter model.collapsible ]
                        , th [ class "important-cell" ] [ text "Budget" ]
                        , th [ class "important-cell" ] [ text "Extra Budget" ]
                        , th [ class "important-cell" ] [ text "Not Billable" ]
                        , th [ class "main-cell" ]
                            [ text "Total Budget" ]
                        ]
                        (requestedGroups
                            |> (List.map (\id -> findRecById (DataID id) dataStorage.groups)
                                    >> Maybe.Extra.values
                                    >> List.map (\( id, group ) -> group.displayName)
                                    >> List.map
                                        (\name ->
                                            [ th [ class "group-start-cell" ] [ text (name ++ " Planned") ]
                                            , th [] [ text (name ++ " Logged Hours") ]
                                            , th [ class "group-end-cell" ] [ text (name ++ " Efficiency") ]
                                            ]
                                        )
                               )
                            |> List.concat
                        )
                    )
                ]
            ]
        ]


footer : List WOExec -> DataStorage -> Html Msg
footer woexecList dataStorage =
    let
        calculateTotal : (a -> Int) -> List a -> Int
        calculateTotal getter =
            List.foldl (\rec acc -> getter rec + acc) 0

        budget =
            calculateTotal
                (\woexec ->
                    case woexec.workOrder of
                        Just wo ->
                            wo.budget

                        Nothing ->
                            0
                )
                woexecList

        extraBudget =
            calculateTotal
                (\woexec ->
                    case woexec.workOrder of
                        Just wo ->
                            wo.extraBudget

                        Nothing ->
                            0
                )
                woexecList

        nonBillable =
            calculateTotal
                (\woexec ->
                    case woexec.workOrder of
                        Just wo ->
                            wo.nonBillable

                        Nothing ->
                            0
                )
                woexecList

        bookedTotal =
            calculateTotal
                .bookedTotal
                woexecList

        loggedTotal =
            calculateTotal
                .loggedTotal
                woexecList

        sum =
            budget + extraBudget + nonBillable
    in
        div [ class "scrolling-table-footer" ]
            [ table [ class "workorders-table scrolling-table" ]
                [ tfoot []
                    [ summaryRow (text "Client Hours Total") dataStorage.groups woexecList
                    , tr [ class "super-footer-row" ]
                        [ th [ class "project-name-cell" ] [ text "Hours Total" ]
                        , td [ class "important-cell" ] [ text <| toHoursValue budget ++ " " ++ Tuple.first timeUnits ]
                        , td [ class "important-cell" ] [ text <| toHoursValue extraBudget ++ " " ++ Tuple.first timeUnits ]
                        , td [ class "important-cell" ] [ text <| toHoursValue nonBillable ++ " " ++ Tuple.first timeUnits ]
                        , td [ class "main-cell" ] [ text <| toHoursValue sum ++ " " ++ Tuple.first timeUnits ]
                        , td [ class "group-total-cell", colspan <| List.length requestedGroups * 3 ]
                            [ div []
                                [ span [] [ text <| "Total Planned: " ++ toHoursValue bookedTotal ++ " " ++ Tuple.first timeUnits ]
                                , span [] [ text <| "Total Logged Hours: " ++ toHoursValue loggedTotal ++ " " ++ Tuple.first timeUnits ]
                                , span [] [ text <| "Total Efficiency: " ++ toHoursValue (loggedTotal // bookedTotal) ++ " " ++ Tuple.first prctUnits ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]


summaryRow : Html Msg -> List ( GroupID, Group ) -> List WOExec -> Html Msg
summaryRow title groups woexecList =
    let
        calculateTotal : (a -> Int) -> List a -> Int
        calculateTotal getter =
            List.foldl (\rec acc -> getter rec + acc) 0

        budget =
            calculateTotal
                (\woexec ->
                    case woexec.workOrder of
                        Just wo ->
                            wo.budget

                        Nothing ->
                            0
                )
                woexecList

        extraBudget =
            calculateTotal
                (\woexec ->
                    case woexec.workOrder of
                        Just wo ->
                            wo.extraBudget

                        Nothing ->
                            0
                )
                woexecList

        nonBillable =
            calculateTotal
                (\woexec ->
                    case woexec.workOrder of
                        Just wo ->
                            wo.nonBillable

                        Nothing ->
                            0
                )
                woexecList

        bookedTotal =
            calculateTotal
                .bookedTotal
                woexecList

        loggedTotal =
            calculateTotal
                .loggedTotal
                woexecList

        sum =
            budget + extraBudget + nonBillable
    in
        tr []
            (List.append
                [ th [ class "project-name-cell" ] [ title ]
                , td [ class "important-cell" ] [ text <| toHoursValue budget ++ " " ++ Tuple.first timeUnits ]
                , td [ class "important-cell" ] [ text <| toHoursValue extraBudget ++ " " ++ Tuple.first timeUnits ]
                , td [ class "important-cell" ] [ text <| toHoursValue nonBillable ++ " " ++ Tuple.first timeUnits ]
                , td [ class "main-cell" ] [ text <| toHoursValue sum ++ " " ++ Tuple.first timeUnits ]
                ]
                (requestedGroups
                    |> List.map
                        (\reqGroupID ->
                            let
                                sumBy : (Data.WorkOrdersExecutive.Group -> Int) -> List WOExec -> Int
                                sumBy getter =
                                    List.foldl
                                        (\rec ->
                                            (+)
                                                (Maybe.withDefault 0
                                                    (rec.groups
                                                        |> List.Extra.find (\group -> reqGroupID == group.groupId)
                                                        >> Maybe.map getter
                                                    )
                                                )
                                        )
                                        0

                                booked =
                                    sumBy .booked woexecList

                                logged =
                                    sumBy .logged woexecList

                                effValue =
                                    if (booked == 0) then
                                        "--"
                                    else
                                        toString (round (toFloat logged / toFloat booked * 100)) ++ " " ++ Tuple.first prctUnits
                            in
                                if (List.member reqGroupID << List.map (\( DataID id, _ ) -> id) <| groups) then
                                    [ td [ class "group-start-cell" ] [ text <| toHoursValue booked ++ " " ++ Tuple.first timeUnits ]
                                    , td [] [ text <| toHoursValue logged ++ " " ++ Tuple.first timeUnits ]
                                    , td [ class "group-end-cell" ] [ text <| effValue ]
                                    ]
                                else
                                    [ nothing ]
                        )
                    |> List.concat
                )
            )


backgroundTable : List ( GroupID, Group ) -> Html Msg
backgroundTable groups =
    table [ class "workorders-table scrolling-table empty-space" ]
        [ tbody []
            [ tr []
                (List.append
                    [ th [ class "project-name-cell" ] []
                    , td [ class "important-cell" ] []
                    , td [ class "important-cell" ] []
                    , td [ class "important-cell" ] []
                    , td [ class "main-cell" ] []
                    ]
                    (List.concat <|
                        List.map2
                            (\reqGroupID ( groupId, _ ) ->
                                [ td [ class "group-start-cell" ] []
                                , td [] []
                                , td [ class "group-end-cell" ] []
                                ]
                            )
                            requestedGroups
                            groups
                    )
                )
            ]
        ]


actionsPanel : Model -> Html Msg
actionsPanel model =
    div [ class "actions-panel" ]
        [ button [ class "prev-month", onClick SubstractMonth ] [ Html.i [ class "fa fa-chevron-left" ] [] ]
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


section : Model -> DataStorage -> (List WOExec -> List (Html Msg)) -> ( ProjectTypeID, ProjectType ) -> Html Msg
section model dataStorage contentList ( (DataID id) as projectTypeID, projectType ) =
    let
        filteredFeed =
            model.woexecFeed
                |> List.filter (funcWithDefault False (\( _, pr ) -> pr.projectType == projectTypeID))
                >> List.sortBy (funcWithDefault "" (\( _, pr ) -> pr.displayName))

        funcWithDefault d f =
            (\wo -> Maybe.map f <| findRecById wo.projectId dataStorage.projects)
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
                    , tfoot [] [ summaryRow (text "Hours SubTotal") dataStorage.groups filteredFeed ]
                    ]
                )
        else
            nothing


rowHeader : List ( ProjectID, Project ) -> WOExec -> Html Msg
rowHeader projects woexec =
    let
        project =
            findRecById woexec.projectId projects

        ( projectName, projectColor ) =
            Maybe.withDefault ( "", "" ) <| Maybe.map (\( id, pr ) -> ( pr.displayName, pr.color )) project

        efficiency =
            if (woexec.bookedTotal == 0) then
                Nothing
            else
                Just <| toFloat (round (toFloat woexec.loggedTotal / toFloat woexec.bookedCurrent * 100)) / 100

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
            woexec.loggedTotal - woexec.bookedCurrent

        message =
            toHoursValue (abs diff) ++ " " ++ Tuple.first timeUnits ++ " " ++ (diff < 0 ? "Behind" <| "Ahead") ++ " Schedule"
    in
        th [ class "project-name-cell" ]
            [ div [ class "efficiency-chart" ]
                (RC.radialChart efficiency projectColor
                    :: Maybe.Extra.values
                        [ Maybe.map RC.indicator effMarker
                        , Maybe.map (\_ -> RC.tooltip message) effMarker
                        ]
                )
            , text projectName
            ]


workOrderSidebar : DataStorage -> WOExec -> Html Msg
workOrderSidebar dataStorage woexec =
    tr [] (List.singleton <| rowHeader dataStorage.projects woexec)


workOrderRow : DataStorage -> WOExec -> Html Msg
workOrderRow { projects, groups } woexec =
    let
        effValue =
            if (woexec.bookedTotal == 0) then
                "--"
            else
                toString (round (toFloat woexec.loggedTotal / toFloat woexec.bookedTotal * 100)) ++ " " ++ Tuple.first prctUnits

        woData =
            case woexec.workOrder of
                Just wo ->
                    { budget = wo.budget, extraBudget = wo.extraBudget, nonBillable = wo.nonBillable }

                Nothing ->
                    { budget = 0, extraBudget = 0, nonBillable = 0 }
    in
        tr []
            (List.concat
                [ [ rowHeader projects woexec
                  , td [ class "important-cell" ] [ text <| toHoursValue woData.budget ++ " " ++ Tuple.first timeUnits ]
                  , td [ class "important-cell" ] [ text <| toHoursValue woData.extraBudget ++ " " ++ Tuple.first timeUnits ]
                  , td [ class "important-cell" ] [ text <| toHoursValue woData.nonBillable ++ " " ++ Tuple.first timeUnits ]
                  , td [ class "main-cell" ] [ text <| toHoursValue (woData.budget + woData.extraBudget + woData.nonBillable) ++ " " ++ Tuple.first timeUnits ]
                  ]
                , requestedGroups
                    |> List.map
                        (\reqGroupID ->
                            if (List.member reqGroupID << List.map (\( DataID id, _ ) -> id) <| groups) then
                                case List.Extra.find (\group -> reqGroupID == group.groupId) woexec.groups of
                                    Just group ->
                                        [ td [ class "group-start-cell" ] [ text <| toHoursValue group.booked ++ " " ++ Tuple.first timeUnits ]
                                        , td [] [ text <| toHoursValue group.logged ++ " " ++ Tuple.first timeUnits ]
                                        , td [ class "group-end-cell" ] [ text <| effValue ]
                                        ]

                                    Nothing ->
                                        [ td [ class "group-start-cell" ] [ text <| "0 " ++ Tuple.first timeUnits ]
                                        , td [] [ text <| "0 " ++ Tuple.first timeUnits ]
                                        , td [ class "group-end-cell" ] [ text <| effValue ]
                                        ]
                            else
                                [ nothing ]
                        )
                    |> List.concat
                ]
            )
