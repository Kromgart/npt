module Data.Resources exposing (..)

-- Service modules

import Exts.Json.Decode exposing (decodeDate)
import Date exposing (Date)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Date.Format exposing (format)


-- Application modules

import Data.Shared exposing (EditableData(..), DataID(..), ProjectID, ResourceID, decodeProjectID, encodeProjectID, decodeResourceID, encodeResourceID)


type alias PlanResRow =
    { resId : ResourceID
    , booked : List PlanResProjectRow
    , capacities : List ( Date, PlanResCapacity )
    }


decodePlanResRow : Decoder PlanResRow
decodePlanResRow =
    field "resourceId" decodeResourceID
        |> andThen
            (\resId ->
                decode PlanResRow
                    |> custom (succeed resId)
                    |> required "booked" (list (decodePlanResProjectRow resId))
                    |> required "capacities" (list (map2 (,) (index 0 decodeDate) (index 1 decodePlanResCapacity)))
            )


type PlanResCapacity
    = Some Int
    | NoneDueSchedule Int
    | NoneDueTimeoff Int
    | ScheduleMissing
    | ResourceInactive Date (Maybe Date)


decodePlanResCapacity : Decoder PlanResCapacity
decodePlanResCapacity =
    field "tag" string
        |> andThen
            (\tag ->
                case tag of
                    "Some" ->
                        decode Some
                            |> required "contents" int

                    "NoneDueSchedule" ->
                        decode NoneDueSchedule
                            |> required "contents" int

                    "NoneDueTimeoff" ->
                        decode NoneDueTimeoff
                            |> required "contents" int

                    "ScheduleMissing" ->
                        decode ScheduleMissing

                    "ResourceInactive" ->
                        field "contents" <|
                            map2 ResourceInactive (index 0 decodeDate) (index 1 <| maybe decodeDate)

                    _ ->
                        fail <|
                            "Error decoding PlanResCapacity: tag "
                                ++ tag
                                ++ " is unknown. Allowed tags are: Some, NoneDueSchedule, NoneDueTimeoff, ScheduleMissing, ResourceInactive"
            )


type alias PlanResProjectRow =
    { projectId : ProjectID
    , bookings : List EBooking
    }


decodePlanResProjectRow : ResourceID -> Decoder PlanResProjectRow
decodePlanResProjectRow resId =
    field "projectId" decodeProjectID
        |> andThen
            (\projId ->
                decode PlanResProjectRow
                    |> custom (succeed projId)
                    |> required "bookings" (list <| decodePlanEBooking resId projId)
            )


type alias BookingID =
    DataID Booking


decodeBookingID : Decoder BookingID
decodeBookingID =
    andThen (succeed << DataID) int


type alias Booking =
    { resourceId : ResourceID
    , bookDate : Date
    , projectId : ProjectID
    , amount : Int
    }


encodeBooking : Booking -> Json.Encode.Value
encodeBooking x =
    Json.Encode.object
        [ ( "resourceId", encodeResourceID x.resourceId )
        , ( "bookDate", (Json.Encode.string << format "%Y-%m-%d") x.bookDate )
        , ( "projectId", encodeProjectID x.projectId )
        , ( "amount", Json.Encode.int x.amount )
        ]


type alias EBooking =
    EditableData ( ResourceID, ProjectID, Date ) Booking


decodePlanEBooking : ResourceID -> ProjectID -> Decoder EBooking
decodePlanEBooking resId projId =
    decode Actual
        |> required "bookingId" decodeBookingID
        |> custom
            (decode Booking
                |> custom (succeed resId)
                |> required "bookingDate" decodeDate
                |> custom (succeed projId)
                |> required "bookingAmount" int
            )
        |> custom
            (decode Booking
                |> custom (succeed resId)
                |> required "bookingDate" decodeDate
                |> custom (succeed projId)
                |> required "bookingAmount" int
            )


decodeBooking : Decoder Booking
decodeBooking =
    decode Booking
        |> required "resourceId" decodeResourceID
        |> required "bookDate" decodeDate
        |> required "projectId" decodeProjectID
        |> required "amount" int



-- type alias PlanResBooking =
--     { bookingId : BookingID
--     , bookingDate : Date
--     , bookingAmount : Int
--     }
-- decodePlanResBooking : Decoder PlanResBooking
-- decodePlanResBooking =
--     decode PlanResBooking
--         |> required "bookingId" decodeBookingID
--         |> required "bookingDate" decodeDate
--         |> required "bookingAmount" int
-- encodePlanResBooking : PlanResBooking -> Json.Encode.Value
-- encodePlanResBooking x =
--     Json.Encode.object
--         [ ( "bookingId", encodeBookingID x.bookingId )
--         , ( "bookingDate", (Json.Encode.string << toString) x.bookingDate )
--         , ( "bookingAmount", Json.Encode.int x.bookingAmount )
--         ]
