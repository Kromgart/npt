module Main exposing (..)

-- Service modules

import Task exposing (Task)
import Http
import Navigation exposing (Location)
import Date exposing (Date)
import List.Extra
import Json.Decode exposing (..)
import Debug exposing (log)


-- Application modules

import Model exposing (Model, Page(..), PageState(..), getData)
import Messages exposing (..)
import Data.Shared exposing (DataStorage, Resource, Project, DateRange, decodeDateRange)
import Request.Shared exposing (serverDomain, getResources, getProjects)
import Router exposing (Route(..), parseLocation, href)
import View exposing (view, getPage)
import Page.Resources as Resources
import Page.Projects as Projects
import Page.WorkOrders as WorkOrders
import Page.WorkOrdersExecutive as WorkOrdersExecutive
import Page.Errored as Errored
import Utils exposing (..)
import Ports


-- Application update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        -- Extract current Page type from app model
        page : Page
        page =
            getPage model.pageState

        -- Lift page actions from page update to app update
        liftActions : ( a, b ) -> (a -> Page) -> (b -> Cmd Msg) -> ( Model, Cmd Msg )
        liftActions ( subModel, subMsg ) page msg =
            ( { model | pageState = Loaded <| page subModel }, msg subMsg )
    in
        case ( msg, page ) of
            ( NoOp, _ ) ->
                ( model, Cmd.none )

            -- Load dataStorage before app load
            ( Init location (Ok dataStorage), _ ) ->
                ( Model (Loaded Blank) dataStorage, setRoute (parseLocation location) )

            ( Init location (Err error), _ ) ->
                ( { model | pageState = Loaded (Errored error) }, Cmd.none )

            -- Handle url and pass it to Router
            ( LocationChange location, page ) ->
                let
                    isEqualRoutes : Maybe Route -> Maybe Route -> Bool
                    isEqualRoutes fromPage toPage =
                        case ( fromPage, toPage ) of
                            -- Resources Routes
                            ( Just ResourcesRouteInit, Just (ResourcesRoute _ _) ) ->
                                True

                            ( Just (ResourcesRoute _ _), Just ResourcesRouteInit ) ->
                                True

                            ( Just (ResourcesRoute _ _), Just (ResourcesRoute _ _) ) ->
                                True

                            -- Projects Routes
                            ( Just ProjectsRoute, Just ProjectsRoute ) ->
                                True

                            -- WorkOrders Routes
                            ( Just WorkOrdersRouteInit, Just (WorkOrdersRoute _ _) ) ->
                                True

                            ( Just (WorkOrdersRoute _ _), Just WorkOrdersRouteInit ) ->
                                True

                            ( Just (WorkOrdersRoute _ _), Just (WorkOrdersRoute _ _) ) ->
                                True

                            -- WorkOrders Routes
                            ( Just WorkOrdersExecutiveRouteInit, Just (WorkOrdersExecutiveRoute _ _) ) ->
                                True

                            ( Just (WorkOrdersExecutiveRoute _ _), Just WorkOrdersExecutiveRouteInit ) ->
                                True

                            ( Just (WorkOrdersExecutiveRoute _ _), Just (WorkOrdersExecutiveRoute _ _) ) ->
                                True

                            -- Disregard wrong interactions
                            ( _, _ ) ->
                                False
                in
                    ( { model | pageState = TransitioningFrom <| getPage model.pageState }
                    , Cmd.batch
                        [ setRoute (parseLocation location)
                        , if isEqualRoutes (getRoute page) (parseLocation location) then
                            Cmd.none
                          else
                            Ports.beforePageLeave ()
                        ]
                    )

            -- Resources Page
            --      Load page initial data
            ( ResourcesMsg (Resources.LoadPage (Ok pageModel)), _ ) ->
                liftActions (Resources.update (Resources.LoadPage (Ok pageModel)) pageModel) Resources (Cmd.map ResourcesMsg)

            ( ResourcesMsg (Resources.LoadPage (Err error)), _ ) ->
                ( { model | pageState = Loaded <| Errored error }, Cmd.none )

            --      Handle page specific messages
            ( ResourcesMsg msg, Resources pageModel ) ->
                liftActions (Resources.update msg pageModel) Resources (Cmd.map ResourcesMsg)

            -- Projects Page
            --      Load page initial data
            ( ProjectsMsg (Projects.LoadPage (Ok pageModel)), _ ) ->
                liftActions (Projects.update (Projects.LoadPage (Ok pageModel)) pageModel) Projects (Cmd.map ProjectsMsg)

            ( ProjectsMsg (Projects.LoadPage (Err error)), _ ) ->
                ( { model | pageState = Loaded <| Errored error }, Cmd.none )

            -- Work Orders Page
            --      Load page initial data
            ( WorkOrdersMsg (WorkOrders.LoadPage (Ok pageModel)), _ ) ->
                liftActions (WorkOrders.update (WorkOrders.LoadPage (Ok pageModel)) pageModel model.dataStorage) WorkOrders (Cmd.map WorkOrdersMsg)

            ( WorkOrdersMsg (WorkOrders.LoadPage (Err error)), _ ) ->
                ( { model | pageState = Loaded <| Errored error }, Cmd.none )

            --      Handle page specific messages
            ( WorkOrdersMsg msg, WorkOrders pageModel ) ->
                liftActions (WorkOrders.update msg pageModel model.dataStorage) WorkOrders (Cmd.map WorkOrdersMsg)

            -- Work Orders Executive Page
            --      Load page initial data
            ( WorkOrdersExecutiveMsg (WorkOrdersExecutive.LoadPage (Ok pageModel)), _ ) ->
                liftActions (WorkOrdersExecutive.update (WorkOrdersExecutive.LoadPage (Ok pageModel)) pageModel model.dataStorage) WorkOrdersExecutive (Cmd.map WorkOrdersExecutiveMsg)

            ( WorkOrdersExecutiveMsg (WorkOrdersExecutive.LoadPage (Err error)), _ ) ->
                ( { model | pageState = Loaded <| Errored error }, Cmd.none )

            --      Handle page specific messages
            ( WorkOrdersExecutiveMsg msg, WorkOrdersExecutive pageModel ) ->
                liftActions (WorkOrdersExecutive.update msg pageModel model.dataStorage) WorkOrdersExecutive (Cmd.map WorkOrdersExecutiveMsg)

            -- 404 Page
            ( NotFoundLoad _, _ ) ->
                ( { model | pageState = Loaded NotFound }, Cmd.none )

            -- Disregard incoming messages that arrived for the wrong page
            ( _, _ ) ->
                ( model, Cmd.none )



-- Application page routing rules


setRoute : Maybe Route -> Cmd Msg
setRoute route =
    case route of
        Just ResourcesRouteInit ->
            Date.now
                |> Task.andThen (mapCurrentMonth Resources.get)
                |> Task.attempt (ResourcesMsg << Resources.LoadPage)

        Just (ResourcesRoute start end) ->
            Task.attempt (ResourcesMsg << Resources.LoadPage) <| Resources.get start end

        Just ProjectsRoute ->
            Task.attempt (ProjectsMsg << Projects.LoadPage) <|
                Projects.get (Date.fromString "2017-09-01") (Date.fromString "2017-09-10")

        Just WorkOrdersRouteInit ->
            Date.now
                |> Task.andThen (mapCurrentMonth WorkOrders.get)
                |> Task.attempt (WorkOrdersMsg << WorkOrders.LoadPage)

        Just (WorkOrdersRoute start end) ->
            Task.attempt (WorkOrdersMsg << WorkOrders.LoadPage) <| WorkOrders.get start end

        Just WorkOrdersExecutiveRouteInit ->
            Date.now
                |> Task.andThen (mapCurrentMonth WorkOrdersExecutive.get)
                |> Task.attempt (WorkOrdersExecutiveMsg << WorkOrdersExecutive.LoadPage)

        Just (WorkOrdersExecutiveRoute start end) ->
            Task.attempt (WorkOrdersExecutiveMsg << WorkOrdersExecutive.LoadPage) <| WorkOrdersExecutive.get start end

        Nothing ->
            Task.perform NotFoundLoad <|
                Task.succeed ()


getRoute : Page -> Maybe Route
getRoute page =
    case page of
        Resources { dateRange } ->
            Just (ResourcesRoute dateRange.startDate dateRange.endDate)

        Projects _ ->
            Just ProjectsRoute

        WorkOrders { dateRange } ->
            Just (WorkOrdersRoute dateRange.startDate dateRange.endDate)

        WorkOrdersExecutive { dateRange } ->
            Just (WorkOrdersExecutiveRoute dateRange.startDate dateRange.endDate)

        Errored _ ->
            Nothing

        NotFound ->
            Nothing

        Blank ->
            Nothing



-- Application subscribtion


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map (ResourcesMsg << Resources.DatepickerChange) datepickerPlanResChange
        , Sub.map (WorkOrdersMsg << WorkOrders.DatepickerChange) datepickerWOChange
        , Sub.map (WorkOrdersExecutiveMsg << WorkOrdersExecutive.DatepickerChange) datepickerWOExecChange
        , Ports.onResTableScroll <| always (ResourcesMsg Resources.TableScroll)
        , Ports.onResTableScroll <| always (WorkOrdersMsg WorkOrders.TableScroll)
        ]


datepickerPlanResChange : Sub (Maybe DateRange)
datepickerPlanResChange =
    Ports.onDatepickerPlanResChange (decodeValue decodeDateRange >> Result.toMaybe)


datepickerWOChange : Sub (Maybe DateRange)
datepickerWOChange =
    Ports.onDatepickerWOChange (decodeValue decodeDateRange >> Result.toMaybe)


datepickerWOExecChange : Sub (Maybe DateRange)
datepickerWOExecChange =
    Ports.onDatepickerWOExecChange (decodeValue decodeDateRange >> Result.toMaybe)



-- Global Init


init : Location -> ( Model, Cmd Msg )
init location =
    ( Model (Loaded Blank) { resources = [], projects = [], projectTypes = [], groups = [] }
    , Task.attempt (Init location) getData
    )



-- Main


main : Program Never Model Msg
main =
    Navigation.program LocationChange
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
