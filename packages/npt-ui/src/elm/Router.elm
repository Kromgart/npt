module Router exposing (..)

-- service modules

import Navigation exposing (Location)
import Html exposing (Attribute)
import Html.Attributes as Attr
import UrlParser exposing (..)
import Date exposing (Date)
import Date.Format exposing (format)
import Debug exposing (log)


-- application page routes


type Route
    = ResourcesRouteInit
    | ResourcesRoute Date Date
    | ProjectsRoute
    | WorkOrdersRouteInit
    | WorkOrdersRoute Date Date
    | WorkOrdersExecutiveRouteInit
    | WorkOrdersExecutiveRoute Date Date



-- map url hash strings to Route type


mapRoute : Parser (Route -> a) a
mapRoute =
    oneOf
        [ map ResourcesRouteInit top
        , map ResourcesRouteInit (s "resources")
        , map ResourcesRoute (s "resources" </> date </> date)
        , map ProjectsRoute (s "projects")
        , map WorkOrdersRouteInit (s "workorders")
        , map WorkOrdersRoute (s "workorders" </> date </> date)
        , map WorkOrdersExecutiveRouteInit (s "workordersexecutive")
        , map WorkOrdersExecutiveRoute (s "workordersexecutive" </> date </> date)
        ]


date : Parser (Date -> a) a
date =
    custom "DATE" Date.fromString



-- transform Route type into url hash strings


routeToString : Route -> String
routeToString route =
    let
        pieces =
            case route of
                ResourcesRouteInit ->
                    [ "resources" ]

                ResourcesRoute s1 s2 ->
                    [ "resources", format "%Y-%m-%d" s1, format "%Y-%m-%d" s2 ]

                ProjectsRoute ->
                    [ "projects" ]

                WorkOrdersRouteInit ->
                    [ "workorders" ]

                WorkOrdersRoute s1 s2 ->
                    [ "workorders", format "%Y-%m-%d" s1, format "%Y-%m-%d" s2 ]

                WorkOrdersExecutiveRouteInit ->
                    [ "workordersexecutive" ]

                WorkOrdersExecutiveRoute s1 s2 ->
                    [ "workordersexecutive", format "%Y-%m-%d" s1, format "%Y-%m-%d" s2 ]
    in
        "#/" ++ String.join "/" pieces



-- get href from Route type


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)



-- transform url hash into Maybe Route type


parseLocation : Location -> Maybe Route
parseLocation location =
    parseHash mapRoute location
