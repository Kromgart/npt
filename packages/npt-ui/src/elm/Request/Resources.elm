module Request.Resources exposing (..)

-- Service modules

import Http
import String
import Date exposing (Date)
import Date.Format exposing (format)
import Json.Decode exposing (..)


-- Application modules

import Data.Shared exposing (DataID(..), EditableData(..), DateRange, encodeDateRange)
import Data.Resources exposing (EBooking, Booking, decodeBooking, BookingID, decodeBookingID, encodeBooking, PlanResRow, decodePlanResRow)


-- Get all bookings in date range and map to EBookings


getBookings : String -> String -> String -> Http.Request (List EBooking)
getBookings urlBase capture_startDate capture_endDate =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                , "api"
                , "v1"
                , "bookings"
                , capture_startDate |> Http.encodeUri
                , capture_endDate |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list (map3 Actual (index 0 decodeBookingID) (index 1 decodeBooking) (index 1 decodeBooking)))
        , timeout =
            Nothing
        , withCredentials =
            False
        }



-- Update booking by passing id and new record


updateBooking : String -> BookingID -> Booking -> Http.Request ()
updateBooking urlBase (DataID capture_bookingId) body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                , "api"
                , "v1"
                , "bookings"
                , capture_bookingId |> toString |> Http.encodeUri
                ]
        , body =
            Http.jsonBody (encodeBooking body)
        , expect =
            Http.expectStringResponse
                (\{ body } ->
                    if body == "[]" then
                        Ok ()
                    else
                        Err "Expected the response body to be empty JSON"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }



-- Create new booking


createBooking : String -> Booking -> Http.Request BookingID
createBooking urlBase body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                , "api"
                , "v1"
                , "bookings"
                ]
        , body =
            Http.jsonBody (encodeBooking body)
        , expect =
            Http.expectJson decodeBookingID
        , timeout =
            Nothing
        , withCredentials =
            False
        }


deleteBooking : String -> BookingID -> Http.Request ()
deleteBooking urlBase (DataID capture_bookingId) =
    Http.request
        { method =
            "DELETE"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                , "api"
                , "v1"
                , "bookings"
                , capture_bookingId |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\{ body } ->
                    if body == "[]" then
                        Ok ()
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }


getPlanningResourcesFeed : String -> Date -> Date -> Http.Request (List PlanResRow)
getPlanningResourcesFeed urlBase capture_startDate capture_endDate =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                , "api"
                , "v1"
                , "tables"
                , "resources"
                , format "%Y-%m-%d" capture_startDate |> Http.encodeUri
                , format "%Y-%m-%d" capture_endDate |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodePlanResRow)
        , timeout =
            Nothing
        , withCredentials =
            False
        }
