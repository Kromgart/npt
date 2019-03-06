module Request.WorkOrders exposing (..)

-- Service modules

import Http
import String
import Date exposing (Date)
import Date.Format exposing (format)
import Json.Decode exposing (..)


-- Application modules

import Data.Shared exposing (..)
import Data.WorkOrders exposing (..)


updateWO : String -> WORowID -> WORow -> Http.Request ()
updateWO urlBase (DataID capture_woId) body =
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
                , "workorders"
                , capture_woId |> toString |> Http.encodeUri
                ]
        , body =
            Http.jsonBody (encodeWORow body)
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


createWO : String -> WORow -> Http.Request WORowID
createWO urlBase body =
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
                , "workorders"
                ]
        , body =
            Http.jsonBody (encodeWORow body)
        , expect =
            Http.expectJson decodeWORowID
        , timeout =
            Nothing
        , withCredentials =
            False
        }


deleteWO : String -> WORowID -> Http.Request ()
deleteWO urlBase (DataID capture_woId) =
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
                , "workorders"
                , capture_woId |> toString |> Http.encodeUri
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


getWorkOrdersFeed : String -> Date -> Date -> Http.Request (List EWORow)
getWorkOrdersFeed urlBase capture_startDate capture_endDate =
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
                , "workorders"
                , format "%Y-%m-%d" capture_startDate |> Http.encodeUri
                , format "%Y-%m-%d" capture_endDate |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeWOTuple)
        , timeout =
            Nothing
        , withCredentials =
            False
        }
