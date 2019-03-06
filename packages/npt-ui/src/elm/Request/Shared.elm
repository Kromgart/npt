module Request.Shared exposing (..)

-- Service modules

import Http
import String
import Json.Decode exposing (..)


-- Application modules

import Data.Shared exposing (..)
import Ports


serverDomain : String
serverDomain =
    ""


datepickerData : DateRange -> Cmd msg
datepickerData dateRange =
    encodeDateRange dateRange
        |> Just
        |> Ports.datepickerData


getResources : String -> Http.Request (List ( ResourceID, Resource ))
getResources urlBase =
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
                , "resources"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list (map2 (,) (index 0 decodeResourceID) (index 1 decodeResource)))
        , timeout =
            Nothing
        , withCredentials =
            False
        }


getProjects : String -> Http.Request (List ( ProjectID, Project ))
getProjects urlBase =
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
                , "projects"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list (map2 (,) (index 0 decodeProjectID) (index 1 decodeProject)))
        , timeout =
            Nothing
        , withCredentials =
            False
        }


getProjectTypes : String -> Http.Request (List ( ProjectTypeID, ProjectType ))
getProjectTypes urlBase =
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
                , "projecttypes"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list (map2 (,) (index 0 decodeProjectTypeID) (index 1 decodeProjectType)))
        , timeout =
            Nothing
        , withCredentials =
            False
        }


getGroups : String -> Http.Request (List ( GroupID, Group ))
getGroups urlBase =
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
                , "groups"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list (map2 (,) (index 0 decodeGroupID) (index 1 decodeGroup)))
        , timeout =
            Nothing
        , withCredentials =
            False
        }



--postApiV1Projects : String -> Project -> Http.Request Int
--postApiV1Projects urlBase body =
--    Http.request
--        { method =
--            "POST"
--        , headers =
--            []
--        , url =
--            String.join "/"
--                [ urlBase
--                , "api"
--                , "v1"
--                , "projects"
--                ]
--        , body =
--            Http.jsonBody (encodeProject body)
--        , expect =
--            Http.expectJson int
--        , timeout =
--            Nothing
--        , withCredentials =
--            False
--        }
--getApiV1Workorders : String -> Http.Request (List ( WorkOrderID, WorkOrder ))
--getApiV1Workorders urlBase =
--    Http.request
--        { method =
--            "GET"
--        , headers =
--            []
--        , url =
--            String.join "/"
--                [ urlBase
--                , "api"
--                , "v1"
--                , "workorders"
--                ]
--        , body =
--            Http.emptyBody
--        , expect =
--            Http.expectJson (list (map2 (,) (index 0 decodeWorkOrderID) (index 1 decodeWorkOrder)))
--        , timeout =
--            Nothing
--        , withCredentials =
--            False
--        }
--
--
--postApiV1Workorders : String -> WorkOrder -> Http.Request WorkOrderID
--postApiV1Workorders urlBase body =
--    Http.request
--        { method =
--            "POST"
--        , headers =
--            []
--        , url =
--            String.join "/"
--                [ urlBase
--                , "api"
--                , "v1"
--                , "workorders"
--                ]
--        , body =
--            Http.jsonBody (encodeWorkOrder body)
--        , expect =
--            Http.expectJson decodeWorkOrderID
--        , timeout =
--            Nothing
--        , withCredentials =
--            False
--        }
