module Page.WorkOrdersExecutive exposing (..)

-- service modules

import Http
import Task exposing (Task)
import Date exposing (Date)
import Date.Format exposing (format)
import Date.Extra exposing (Interval(..))
import List.Extra
import Maybe.Extra
import Navigation
import DOM
import Debug exposing (log)
import Set exposing (Set)


-- application modules

import Data.Shared exposing (DataStorage, WorkOrder, WorkOrderID, DateRange, EditableData(..), DataID(..), ProjectID)
import Data.WorkOrdersExecutive exposing (WOExec)
import Request.Shared exposing (serverDomain, datepickerData)
import Request.WorkOrdersExecutive exposing (..)
import Utils exposing (..)
import Ports
import Module.Collapsible as CLPS


requestedGroups : List Int
requestedGroups =
    [ 1, 2 ]



-- define work orders page model


type alias Model =
    { dateRange : DateRange
    , collapsible : CLPS.Model
    , woexecFeed : List WOExec
    }



-- get work orders page model


get : Date -> Date -> Task Http.Error Model
get startDate endDate =
    getWOExecFeed serverDomain startDate endDate requestedGroups
        |> Http.toTask
        |> Task.map (Model (DateRange startDate endDate) (CLPS.init Set.empty []))


type Msg
    = NoOp
    | LoadPage (Result Http.Error Model)
    | DatepickerChange (Maybe DateRange)
    | AddMonth
    | SubstractMonth
    | CLPS CLPS.Msg -- Collapsible module message lift


update : Msg -> Model -> DataStorage -> ( Model, Cmd Msg )
update msg model dataStorage =
    let
        -- Lift page actions from module update to page update
        liftActions : (a -> Model -> Model) -> ( a, b ) -> (b -> Cmd Msg) -> ( Model, Cmd Msg )
        liftActions setter ( subModel, subMsg ) msg =
            ( setter subModel model, msg subMsg )
    in
        case msg of
            NoOp ->
                ( model, Cmd.none )

            LoadPage (Ok newModel) ->
                let
                    getProject wo =
                        findRecById wo.projectId dataStorage.projects

                    getProjectTypeId ( _, project ) =
                        project.projectType

                    getType id =
                        findRecById id dataStorage.projectTypes

                    flatten =
                        (\( DataID id, projectType ) -> ( DataID id, projectType.displayName ))

                    id =
                        (\( DataID id, projectType ) -> id)

                    model =
                        { newModel
                            | collapsible =
                                newModel.woexecFeed
                                    |> List.map
                                        (getProject
                                            >> Maybe.map getProjectTypeId
                                            >> Maybe.andThen getType
                                        )
                                    >> Maybe.Extra.values
                                    >> List.map flatten
                                    >> List.Extra.uniqueBy id
                                    >> CLPS.init Set.empty
                        }
                in
                    ( model, Cmd.batch [ datepickerData newModel.dateRange, Ports.pageLoad () ] )

            LoadPage (Err _) ->
                ( model, Cmd.none )

            DatepickerChange (Just { startDate, endDate }) ->
                ( model
                , Navigation.newUrl
                    ("#/workordersexecutive/"
                        ++ format "%Y-%m-%d" startDate
                        ++ "/"
                        ++ format "%Y-%m-%d" endDate
                    )
                )

            DatepickerChange Nothing ->
                ( model, Cmd.none )

            SubstractMonth ->
                ( model
                , Navigation.newUrl
                    ("#/workordersexecutive/"
                        ++ format "%Y-%m-%d" (Date.Extra.add Month -1 model.dateRange.startDate)
                        ++ "/"
                        ++ format "%Y-%m-%d" (Date.Extra.add Month -1 model.dateRange.endDate)
                    )
                )

            AddMonth ->
                ( model
                , Navigation.newUrl
                    ("#/workordersexecutive/"
                        ++ format "%Y-%m-%d" (Date.Extra.add Month 1 model.dateRange.startDate)
                        ++ "/"
                        ++ format "%Y-%m-%d" (Date.Extra.add Month 1 model.dateRange.endDate)
                    )
                )

            -- Lift Collapsible default messages
            CLPS msg ->
                liftActions (\subModel model -> { model | collapsible = subModel }) (CLPS.update msg model.collapsible) (Cmd.map CLPS)


changeAmount : Int -> String -> Int
changeAmount oldValue newValue =
    case String.toFloat newValue of
        Ok newAmount ->
            (round (roundToFirstSign newAmount * 60)) // 30 * 30

        Err _ ->
            if String.isEmpty newValue then
                0
            else
                oldValue
