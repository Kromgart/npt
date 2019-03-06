module Data.WorkOrders exposing (..)

-- Service modules

import Exts.Json.Decode exposing (decodeDate)
import Date exposing (Date)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Date.Format exposing (format)


-- Application modules

import Data.Shared exposing (..)


type alias WORowID =
    DataID WORow


decodeWORowID : Decoder WORowID
decodeWORowID =
    andThen (succeed << DataID) int



--
--encodeWORowID : WORowID -> Json.Encode.Value
--encodeWORowID (DataID a) =
--    Json.Encode.int a


type alias WORow =
    { projectId : ProjectID
    , budget : Int
    , extraBudget : Int
    , nonBillable : Int
    , startDate : Date
    , endDate : Date
    , isApproved : Bool
    , loggedTotal : Int
    , bookedTotal : Int
    , bookedCurrent : Int
    }


decodeWOTuple : Decoder EWORow
decodeWOTuple =
    resolve <|
        map3
            (\loggedTotal bookedTotal bookedCurrent ->
                field "workOrder" <|
                    map3 Actual
                        (index 0 decodeWORowID)
                        (index 1 <| decodeWORow loggedTotal bookedTotal bookedCurrent)
                        (index 1 <| decodeWORow loggedTotal bookedTotal bookedCurrent)
            )
            (field "logged" int)
            (field "bookedTotal" int)
            (field "bookedCurrent" int)


decodeWORow : Int -> Int -> Int -> Decoder WORow
decodeWORow loggedTotal bookedTotal bookedCurrent =
    decode WORow
        |> required "projectId" decodeProjectID
        |> required "budget" int
        |> required "extraBudget" int
        |> required "nonBillable" int
        |> required "startDate" decodeDate
        |> required "endDate" decodeDate
        |> required "isApproved" bool
        |> hardcoded loggedTotal
        |> hardcoded bookedTotal
        |> hardcoded bookedCurrent


encodeWORow : WORow -> Json.Encode.Value
encodeWORow x =
    Json.Encode.object
        [ ( "projectId", encodeProjectID x.projectId )
        , ( "budget", Json.Encode.int x.budget )
        , ( "extraBudget", Json.Encode.int x.extraBudget )
        , ( "nonBillable", Json.Encode.int x.nonBillable )
        , ( "startDate", (Json.Encode.string << format "%Y-%m-%d") x.startDate )
        , ( "endDate", (Json.Encode.string << format "%Y-%m-%d") x.endDate )
        , ( "isApproved", Json.Encode.bool x.isApproved )
        ]


type alias EWORow =
    EditableData () WORow



--decodeWORow : Decoder WORow
--decodeWORow =
--    decode WORow
--        |> required "projectId" decodeProjectID
--        |> required "budget" int
--        |> required "extraBudget" int
--        |> required "nonBillable" int
--        |> required "startDate" decodeDate
--        |> required "endDate" decodeDate
--        |> required "isApproved" bool
--    decode WORow
--        |> required "workOrder" (map2 (,) (index 0 decodeWORowID) (index 1 decodeWorkOrder))
--    field "resourceId" decodeResourceID
--        |> andThen
--            (\resId ->
--                decode PlanResRow
--                    |> custom (succeed resId)
--                    |> required "booked" (list (decodePlanResProjectRow resId))
--                    |> required "capacities" (list (map2 (,) (index 0 decodeDate) (index 1 decodePlanResCapacity)))
--            )
