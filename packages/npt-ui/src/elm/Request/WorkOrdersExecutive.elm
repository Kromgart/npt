module Request.WorkOrdersExecutive exposing (..)

-- Service modules

import Http
import String
import Date exposing (Date)
import Date.Format exposing (format)
import Json.Decode exposing (..)


-- Application modules

import Data.WorkOrdersExecutive exposing (..)


getWOExecFeed : String -> Date -> Date -> List Int -> Http.Request (List WOExec)
getWOExecFeed urlBase capture_startDate capture_endDate groups =
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
                , "tables"
                , "workordersexec"
                , format "%Y-%m-%d" capture_startDate |> Http.encodeUri
                , format "%Y-%m-%d" capture_endDate |> Http.encodeUri
                ]
        , body =
            Http.jsonBody <| encodeGroupIDs groups
        , expect =
            Http.expectJson <| list decodeWOExec
        , timeout =
            Nothing
        , withCredentials =
            False
        }
