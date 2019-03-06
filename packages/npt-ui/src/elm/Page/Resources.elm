module Page.Resources exposing (..)

-- Service modules

import Navigation exposing (Location)
import Task exposing (Task)
import Http
import Date exposing (Date)
import List.Extra
import Set exposing (Set)
import Date.Format exposing (format)
import Debug exposing (log)
import DOM


-- Application modules

import Utils exposing (..)
import Data.Shared exposing (DataID(..), EditableData(..), ResourceID, ProjectID, DateRange)
import Data.Resources exposing (EBooking, Booking, BookingID, PlanResRow, PlanResProjectRow)
import Request.Shared exposing (serverDomain, datepickerData)
import Request.Resources exposing (getPlanningResourcesFeed, createBooking, updateBooking, deleteBooking)
import Ports


-- Define people page model


type RowState
    = Capacity
    | Available
    | Efficiency


type alias RowFilterState =
    { rows : List String
    , selected : Set String
    }


type alias AssignDropdown =
    { currentResource : Maybe PlanResRow
    , filter : String
    , rect : DOM.Rectangle
    }


type alias Model =
    { dateRange : DateRange
    , rowFilterState : RowFilterState
    , assignDropdown : AssignDropdown
    , planningFeed : List PlanResRow
    }



-- Get people page model


get : Date -> Date -> Task Http.Error Model
get startDate endDate =
    getPlanningResourcesFeed serverDomain startDate endDate
        |> Http.toTask
        |> Task.map
            (Model
                (DateRange startDate endDate)
                (RowFilterState [ "Capacity", "Available", "Efficiency" ] <| Set.singleton "Available")
                (AssignDropdown Nothing "" <| DOM.Rectangle 0 0 0 0)
            )



-- Resources message type


type Msg
    = NoOp
      -- EBooking state transition actions
    | BookingInput EBooking String
    | BookingCreate EBooking
    | BookingCreateResponse EBooking (Result Http.Error BookingID)
    | BookingUpdate EBooking
    | BookingUpdateResponse EBooking (Result Http.Error ())
    | RowFilterSelected String Bool
    | ShowProjectsList (Maybe PlanResRow) DOM.Rectangle
    | AddPlanResProjectRow ResourceID ProjectID
    | InputNewProjectFilter String
    | DatepickerChange (Maybe DateRange)
    | TableScroll
    | LoadPage (Result Http.Error Model)



-- Resources page update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LoadPage (Ok newModel) ->
            ( fillWithEmptyBookings newModel, Cmd.batch [ datepickerData newModel.dateRange, Ports.pageLoad () ] )

        LoadPage (Err _) ->
            ( model, Cmd.none )

        DatepickerChange (Just { startDate, endDate }) ->
            ( model, Navigation.newUrl ("#/resources/" ++ format "%Y-%m-%d" startDate ++ "/" ++ format "%Y-%m-%d" endDate) {- Task.attempt UpdatePage <| get startDate endDate -} )

        DatepickerChange Nothing ->
            ( model, Cmd.none )

        TableScroll ->
            ( { model | assignDropdown = AssignDropdown Nothing "" <| DOM.Rectangle 0 0 0 0 }, Cmd.none )

        ShowProjectsList Nothing _ ->
            ( { model | assignDropdown = AssignDropdown Nothing "" <| DOM.Rectangle 0 0 0 0 }, Cmd.none )

        ShowProjectsList resource rect ->
            let
                assignDropdown =
                    model.assignDropdown

                newResource =
                    if assignDropdown.currentResource == Nothing || assignDropdown.currentResource /= resource then
                        resource
                    else
                        Nothing
            in
                ( { model | assignDropdown = AssignDropdown newResource "" rect }, Cmd.none )

        AddPlanResProjectRow resourceId projectId ->
            let
                planningFeed =
                    List.map
                        (\resource ->
                            if resource.resId == resourceId then
                                { resource
                                    | booked =
                                        PlanResProjectRow projectId (List.map (\date -> Empty ( resourceId, projectId, date )) <| createDateList model.dateRange.startDate model.dateRange.endDate)
                                            :: resource.booked
                                }
                            else
                                resource
                        )
                        model.planningFeed
            in
                ( { model | planningFeed = planningFeed, assignDropdown = AssignDropdown Nothing "" <| DOM.Rectangle 0 0 0 0 }, Cmd.none )

        InputNewProjectFilter filter ->
            let
                assignDropdown =
                    model.assignDropdown

                newProjectDropdown_ =
                    { assignDropdown | filter = filter }
            in
                ( { model | assignDropdown = newProjectDropdown_ }, Cmd.none )

        RowFilterSelected name state ->
            let
                oldRowFilterState =
                    model.rowFilterState

                newRowFilterState =
                    { oldRowFilterState
                        | selected =
                            if state then
                                Set.insert name oldRowFilterState.selected
                            else
                                Set.remove name oldRowFilterState.selected
                    }
            in
                ( { model
                    | rowFilterState = newRowFilterState
                  }
                , Cmd.none
                )

        BookingCreate (Empty ( resourceId, projectId, bookDate )) ->
            -- Create new booking in local model. State change Empty -> New
            ( transformEBookingState model
                (Empty ( resourceId, projectId, bookDate ))
                (New <| Booking resourceId bookDate projectId 0)
            , Cmd.none
            )

        BookingInput (Actual bookingId bRec cachedRec) value ->
            -- Update existing booking in local model. State change Actual -> Actual
            let
                newBRec =
                    changeAmount bRec value
            in
                ( transformEBookingState model
                    (Actual bookingId bRec cachedRec)
                    (Actual bookingId newBRec cachedRec)
                , Cmd.none
                )

        BookingInput (New bRec) value ->
            -- Update non-existing(new) booking in local model. State change New -> New
            let
                newBRec =
                    changeAmount bRec value
            in
                ( transformEBookingState model
                    (New bRec)
                    (New newBRec)
                , Cmd.none
                )

        BookingUpdate (Actual bookingId bRec cachedRec) ->
            if bRec.amount > 0 then
                -- Update existing booking in database. State change Actual -> Updating
                if bRec /= cachedRec then
                    ( transformEBookingState model
                        (Actual bookingId bRec cachedRec)
                        (Updating bookingId bRec cachedRec)
                    , Http.send (BookingUpdateResponse (Updating bookingId bRec cachedRec)) <| updateBooking serverDomain bookingId bRec
                    )
                else
                    ( model, Cmd.none )
            else
                -- Delete existing booking from database. State change Actual -> Deleting
                ( transformEBookingState model
                    (Actual bookingId bRec cachedRec)
                    (Deleting bookingId bRec cachedRec)
                , Http.send (BookingUpdateResponse (Deleting bookingId bRec cachedRec)) <| deleteBooking serverDomain bookingId
                )

        BookingUpdate (New bRec) ->
            if bRec.amount > 0 then
                -- Create new booking in database. State change New -> Creating
                ( transformEBookingState model
                    (New bRec)
                    (Creating bRec)
                , Http.send (BookingCreateResponse (Creating bRec)) <| createBooking serverDomain bRec
                )
            else
                -- Delete non-existing(new) booking from local model. State change New -> Empty
                ( transformEBookingState model
                    (New bRec)
                    (Empty ( bRec.resourceId, bRec.projectId, bRec.bookDate ))
                , Cmd.none
                )

        BookingCreateResponse (Creating bRec) (Ok bookingId) ->
            -- Create new booking in database. State change Creating -> Actual
            ( transformEBookingState model
                (Creating bRec)
                (Actual bookingId bRec bRec)
            , Cmd.none
            )

        BookingCreateResponse (Creating bRec) (Err message) ->
            ( transformEBookingState model
                (Creating bRec)
                (Error (toString message) (New bRec))
            , Cmd.none
            )

        BookingUpdateResponse (Updating bookingId bRec cachedRec) (Ok _) ->
            -- Update existing booking in local model. State change Updating -> Actual
            ( transformEBookingState model
                (Updating bookingId bRec cachedRec)
                (Actual bookingId bRec bRec)
            , Cmd.none
            )

        BookingUpdateResponse (Updating bookingId bRec cachedRec) (Err message) ->
            ( transformEBookingState model
                (Updating bookingId bRec cachedRec)
                (Error (toString message) (Actual bookingId bRec cachedRec))
            , Cmd.none
            )

        BookingUpdateResponse (Deleting bookingId bRec cachedRec) (Ok _) ->
            -- Delete existing booking from local model. State change Deleting -> Empty
            ( transformEBookingState model
                (Deleting bookingId bRec cachedRec)
                (Empty ( bRec.resourceId, bRec.projectId, bRec.bookDate ))
            , Cmd.none
            )

        BookingUpdateResponse (Deleting bookingId bRec cachedRec) (Err message) ->
            -- Delete existing booking from local model. State change Deleting -> Empty
            ( transformEBookingState model
                (Deleting bookingId bRec cachedRec)
                (Error (toString message) (Actual bookingId cachedRec cachedRec))
            , Cmd.none
            )

        -- Disregard wrong state transitions
        BookingCreate _ ->
            ( model, Cmd.none )

        BookingInput _ _ ->
            ( model, Cmd.none )

        BookingUpdate _ ->
            ( model, Cmd.none )

        BookingCreateResponse _ (Err message) ->
            ( model, Cmd.none )

        BookingCreateResponse _ _ ->
            ( model, Cmd.none )

        BookingUpdateResponse _ (Ok _) ->
            ( model, Cmd.none )

        BookingUpdateResponse _ (Err message) ->
            ( model, Cmd.none )



-- Update state


transformEBookingState : Model -> EBooking -> EBooking -> Model
transformEBookingState model bOld bNew =
    { model
        | planningFeed =
            List.map
                (\res ->
                    { res
                        | booked =
                            List.map
                                (\proj ->
                                    { proj
                                        | bookings =
                                            List.map
                                                (\bLoc ->
                                                    if bLoc == bOld then
                                                        bNew
                                                    else
                                                        bLoc
                                                )
                                                proj.bookings
                                    }
                                )
                                res.booked
                    }
                )
                model.planningFeed
    }



-- Fill model with "Empty" EBooking states where no Actual bookings were recieved from server


fillWithEmptyBookings : Model -> Model
fillWithEmptyBookings ({ dateRange, planningFeed } as model) =
    let
        filledBookings resourceId projectId bookings =
            List.map
                (\date ->
                    let
                        bookedDate =
                            List.Extra.find
                                (\booking ->
                                    case booking of
                                        Actual _ bookRec _ ->
                                            equalDateNoTime bookRec.bookDate date

                                        _ ->
                                            False
                                )
                                bookings
                    in
                        case bookedDate of
                            Just date ->
                                date

                            Nothing ->
                                Empty ( resourceId, projectId, date )
                )
            <|
                createDateList model.dateRange.startDate model.dateRange.endDate
    in
        { model
            | planningFeed =
                List.map
                    (\resource ->
                        { resource
                            | booked =
                                List.map
                                    (\project ->
                                        { project
                                            | bookings = filledBookings resource.resId project.projectId project.bookings
                                        }
                                    )
                                    resource.booked
                        }
                    )
                    planningFeed
        }



-- Update booking record amount field


changeAmount : Booking -> String -> Booking
changeAmount bRec value =
    case String.toFloat value of
        Ok newAmount ->
            let
                calculatedAmount =
                    (round (roundToFirstSign newAmount * 60)) // 30 * 30
            in
                { bRec
                    | amount =
                        if calculatedAmount > 24 * 60 then
                            24 * 60
                        else
                            calculatedAmount
                }

        Err _ ->
            if String.isEmpty value then
                { bRec | amount = 0 }
            else
                bRec
