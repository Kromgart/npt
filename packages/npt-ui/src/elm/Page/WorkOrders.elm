module Page.WorkOrders exposing (..)

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
import Data.WorkOrders exposing (WORowID, WORow, EWORow)
import Request.Shared exposing (serverDomain, datepickerData)
import Request.WorkOrders exposing (getWorkOrdersFeed, updateWO, createWO, deleteWO)
import Utils exposing (..)
import Module.SelectWithSearch as SWS
import Ports
import Module.Collapsible as CLPS


-- define work orders page model


type alias Model =
    { dateRange : DateRange
    , selectProject : SWS.Model
    , collapsible : CLPS.Model
    , newWOState : Maybe EWORow
    , woFeed : List EWORow
    }


type alias NewWOState =
    { ewo : EWORow
    }



-- get work orders page model


get : Date -> Date -> Task Http.Error Model
get startDate endDate =
    getWorkOrdersFeed serverDomain startDate endDate
        |> Http.toTask
        |> Task.map (Model (DateRange startDate endDate) (SWS.init Nothing) (CLPS.init Set.empty []) Nothing)


type Msg
    = NoOp
    | LoadPage (Result Http.Error Model)
    | DatepickerChange (Maybe DateRange)
    | EWORowCreate
    | EWORowDelete EWORow
    | EWORowInput (WORow -> String -> WORow) EWORow String
    | EWORowCheck (WORow -> Bool -> WORow) EWORow Bool
    | EWORowUpdate EWORow
    | EWORowCancel
    | EWORowUpdateResponse EWORow (Result Http.Error ())
    | EWORowCreateResponse EWORow (Result Http.Error WORowID)
    | AddMonth
    | SubstractMonth
    | TableScroll
    | SWS SWS.Msg -- SelectWithSearch module message lift
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
                                newModel.woFeed
                                    |> List.map (extractWO >> Maybe.andThen getProject >> Maybe.map getProjectTypeId >> Maybe.andThen getType)
                                    |> (Maybe.Extra.values >> List.map flatten >> List.Extra.uniqueBy id)
                                    |> CLPS.init Set.empty
                        }
                in
                    ( model, Cmd.batch [ datepickerData newModel.dateRange, Ports.pageLoad () ] )

            LoadPage (Err _) ->
                ( model, Cmd.none )

            DatepickerChange (Just { startDate, endDate }) ->
                ( model, Navigation.newUrl ("#/workorders/" ++ format "%Y-%m-%d" startDate ++ "/" ++ format "%Y-%m-%d" endDate) {- Task.attempt UpdatePage <| get startDate endDate -} )

            DatepickerChange Nothing ->
                ( model, Cmd.none )

            SubstractMonth ->
                ( model
                , Navigation.newUrl
                    ("#/workorders/"
                        ++ format "%Y-%m-%d" (Date.Extra.add Month -1 model.dateRange.startDate)
                        ++ "/"
                        ++ format "%Y-%m-%d" (Date.Extra.add Month -1 model.dateRange.endDate)
                    )
                )

            AddMonth ->
                ( model
                , Navigation.newUrl
                    ("#/workorders/"
                        ++ format "%Y-%m-%d" (Date.Extra.add Month 1 model.dateRange.startDate)
                        ++ "/"
                        ++ format "%Y-%m-%d" (Date.Extra.add Month 1 model.dateRange.endDate)
                    )
                )

            -- Send message to SelectWithSearch module to close the dropdown
            TableScroll ->
                ( model, Task.perform SWS <| Task.succeed SWS.OnClose )

            -- Capture SelectWithSearch Selected message
            SWS (SWS.Selected prid) ->
                let
                    newWOState =
                        Maybe.map
                            (\ewo ->
                                case ewo of
                                    New wo ->
                                        New { wo | projectId = prid }

                                    _ ->
                                        ewo
                            )
                            model.newWOState
                in
                    ( { model | newWOState = newWOState }, Cmd.none )

            -- Lift SelectWithSearch default messages
            SWS msg ->
                liftActions (\subModel model -> { model | selectProject = subModel }) (SWS.update msg model.selectProject) (Cmd.map SWS)

            -- Lift Collapsible default messages
            CLPS msg ->
                liftActions (\subModel model -> { model | collapsible = subModel }) (CLPS.update msg model.collapsible) (Cmd.map CLPS)

            EWORowCreate ->
                let
                    newWOState =
                        Just <| New <| WORow (DataID 0) 0 0 0 model.dateRange.startDate model.dateRange.endDate False 0 0 0
                in
                    ( { model | newWOState = newWOState }, Cmd.none )

            EWORowCancel ->
                ( { model | newWOState = Nothing }, Cmd.none )

            EWORowInput setter (New wo) value ->
                let
                    newWo =
                        setter wo value
                in
                    ( transformNewWORowState model
                        (New wo)
                        (New newWo)
                    , Cmd.none
                    )

            EWORowInput setter (Actual woId wo cachedWo) value ->
                let
                    newWo =
                        setter wo value
                in
                    ( transformEWORowState model
                        (Actual woId wo cachedWo)
                        (Updating woId newWo cachedWo)
                    , Http.send (EWORowUpdateResponse (Updating woId newWo cachedWo)) <| updateWO serverDomain woId newWo
                    )

            EWORowCheck setter (New wo) value ->
                let
                    newWo =
                        setter wo value
                in
                    ( transformNewWORowState model
                        (New wo)
                        (New newWo)
                    , Cmd.none
                    )

            EWORowCheck setter (Actual woId wo cachedWo) value ->
                let
                    newWo =
                        setter wo value
                in
                    ( transformEWORowState model
                        (Actual woId wo cachedWo)
                        (Updating woId newWo cachedWo)
                    , Http.send (EWORowUpdateResponse (Updating woId newWo cachedWo)) <| updateWO serverDomain woId newWo
                    )

            EWORowUpdate (Actual woId wo cachedWo) ->
                if wo /= cachedWo then
                    ( transformEWORowState model
                        (Actual woId wo cachedWo)
                        (Updating woId wo cachedWo)
                    , Http.send (EWORowUpdateResponse (Updating woId wo cachedWo)) <| updateWO serverDomain woId wo
                    )
                else
                    ( model, Cmd.none )

            EWORowUpdate (New wo) ->
                ( transformEWORowState { model | selectProject = SWS.init Nothing }
                    (New wo)
                    (Creating wo)
                , Http.send (EWORowCreateResponse (Creating wo)) <| createWO serverDomain wo
                )

            EWORowUpdateResponse (Updating woId wo cachedWo) (Ok _) ->
                -- Update existing booking in local model. State change Updating -> Actual
                ( transformEWORowState model
                    (Updating woId wo cachedWo)
                    (Actual woId wo wo)
                , Cmd.none
                )

            EWORowUpdateResponse (Updating bookingId bRec cachedRec) (Err message) ->
                ( transformEWORowState model
                    (Updating bookingId bRec cachedRec)
                    (Error (toString message) (Actual bookingId bRec cachedRec))
                , Cmd.none
                )

            EWORowUpdateResponse (Deleting woId wo cachedWo) (Ok _) ->
                -- Update existing booking in local model. State change Updating -> Actual
                ( { model | woFeed = List.Extra.remove (Deleting woId wo cachedWo) model.woFeed }
                , Cmd.none
                )

            EWORowUpdateResponse (Deleting bookingId bRec cachedRec) (Err message) ->
                ( transformEWORowState model
                    (Deleting bookingId bRec cachedRec)
                    (Error (toString message) (Actual bookingId bRec cachedRec))
                , Cmd.none
                )

            EWORowCreateResponse (Creating wo) (Ok woId) ->
                -- Create new booking in database. State change Creating -> Actual
                ( { model | newWOState = Nothing, woFeed = (Actual woId wo wo) :: model.woFeed }
                , Cmd.none
                )

            EWORowCreateResponse (Creating wo) (Err message) ->
                ( transformEWORowState model
                    (Creating wo)
                    (Error (toString message) (New wo))
                , Cmd.none
                )

            EWORowDelete (Actual woId wo cachedWo) ->
                ( transformEWORowState model
                    (Actual woId wo cachedWo)
                    (Deleting woId wo cachedWo)
                , Http.send (EWORowUpdateResponse (Deleting woId wo cachedWo)) <| deleteWO serverDomain woId
                )

            EWORowInput _ _ _ ->
                ( model, Cmd.none )

            EWORowCheck _ _ _ ->
                ( model, Cmd.none )

            EWORowUpdate _ ->
                ( model, Cmd.none )

            EWORowUpdateResponse _ _ ->
                ( model, Cmd.none )

            EWORowCreateResponse _ _ ->
                ( model, Cmd.none )

            EWORowDelete _ ->
                ( model, Cmd.none )


setBudget : WORow -> String -> WORow
setBudget wo value =
    { wo | budget = changeAmount wo.budget value }


setExtBudget : WORow -> String -> WORow
setExtBudget wo value =
    { wo | extraBudget = changeAmount wo.extraBudget value }


setNonBudget : WORow -> String -> WORow
setNonBudget wo value =
    { wo | nonBillable = changeAmount wo.nonBillable value }


setApprove : WORow -> Bool -> WORow
setApprove wo value =
    { wo | isApproved = value }


transformNewWORowState : Model -> EWORow -> EWORow -> Model
transformNewWORowState model ewoOld ewoNew =
    { model
        | newWOState =
            Maybe.map
                (\ewoLoc ->
                    if ewoLoc == ewoOld then
                        ewoNew
                    else
                        ewoLoc
                )
                model.newWOState
    }


transformEWORowState : Model -> EWORow -> EWORow -> Model
transformEWORowState model ewoOld ewoNew =
    { model
        | woFeed =
            List.map
                (\ewoLoc ->
                    if ewoLoc == ewoOld then
                        ewoNew
                    else
                        ewoLoc
                )
                model.woFeed
    }


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


extractWO : EWORow -> Maybe WORow
extractWO eWORow =
    case eWORow of
        New wo ->
            Just wo

        Actual _ wo _ ->
            Just wo

        Updating _ wo _ ->
            Just wo

        Deleting _ wo _ ->
            Just wo

        Error _ (Actual _ wo _) ->
            Just wo

        Error _ (New wo) ->
            Just wo

        _ ->
            Nothing
