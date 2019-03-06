module Data.WorkOrdersExecutive exposing (..)

-- Service modules

import Exts.Json.Decode exposing (decodeDate)
import Date exposing (Date)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Date.Format exposing (format)


-- Application modules

import Data.Shared exposing (..)


--
--encodeWORowID : WORowID -> Json.Encode.Value
--encodeWORowID (DataID a) =
--    Json.Encode.int a


type alias WOExec =
    { projectId : ProjectID
    , loggedTotal : Int
    , bookedTotal : Int
    , bookedCurrent : Int
    , workOrder : Maybe WO
    , groups : List Group
    }


type alias WO =
    { workOrderId : Int
    , budget : Int
    , extraBudget : Int
    , nonBillable : Int
    }


type alias Group =
    { groupId : Int
    , booked : Int
    , logged : Int
    }


decodeWOExec : Decoder WOExec
decodeWOExec =
    decode WOExec
        |> required "projectId" decodeProjectID
        |> required "loggedTotal" int
        |> required "bookedTotal" int
        |> required "bookedCurrent" int
        |> required "workOrder" (maybe decodeWO)
        |> required "groups" (list decodeGroup)


decodeWO : Decoder WO
decodeWO =
    decode WO
        |> required "workOrderId" int
        |> required "budget" int
        |> required "extraBudget" int
        |> required "nonBillable" int


decodeGroup : Decoder Group
decodeGroup =
    decode Group
        |> required "groupId" int
        |> required "booked" int
        |> required "logged" int


encodeGroupIDs : List Int -> Json.Encode.Value
encodeGroupIDs list =
    Json.Encode.list <| List.map Json.Encode.int list
