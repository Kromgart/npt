module Autogenerated.WebAPI exposing (..)

import Exts.Json.Decode exposing (decodeDate)
import Date exposing (Date)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type alias DataId =
    { runId : Int
    }


decodeDataId : Decoder DataId
decodeDataId =
    decode DataId
        |> required "runId" int


encodeDataId : DataId -> Json.Encode.Value
encodeDataId x =
    Json.Encode.object
        [ ( "runId", Json.Encode.int x.runId )
        ]


type alias Resource =
    { externalId : Maybe Int
    , supervisor : Maybe DataId
    , name : String
    , schedule : DataId
    , startDate : Date
    , endDate : Maybe Date
    , title : String
    , groups : List Int
    , email : Maybe String
    , phone : Maybe String
    }


decodeResource : Decoder Resource
decodeResource =
    decode Resource
        |> required "externalId" (maybe int)
        |> required "supervisor" (maybe decodeDataId)
        |> required "name" string
        |> required "schedule" decodeDataId
        |> required "startDate" decodeDate
        |> required "endDate" (maybe decodeDate)
        |> required "title" string
        |> required "groups" (list int)
        |> required "email" (maybe string)
        |> required "phone" (maybe string)


encodeResource : Resource -> Json.Encode.Value
encodeResource x =
    Json.Encode.object
        [ ( "externalId", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.int) x.externalId )
        , ( "supervisor", (Maybe.withDefault Json.Encode.null << Maybe.map encodeDataId) x.supervisor )
        , ( "name", Json.Encode.string x.name )
        , ( "schedule", encodeDataId x.schedule )
        , ( "startDate", (Json.Encode.string << toString) x.startDate )
        , ( "endDate", (Maybe.withDefault Json.Encode.null << Maybe.map (Json.Encode.string << toString)) x.endDate )
        , ( "title", Json.Encode.string x.title )
        , ( "groups", (Json.Encode.list << List.map Json.Encode.int) x.groups )
        , ( "email", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.email )
        , ( "phone", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.phone )
        ]


type alias Project =
    { externalId : Maybe Int
    , fullName : String
    , displayName : String
    , color : String
    }


decodeProject : Decoder Project
decodeProject =
    decode Project
        |> required "externalId" (maybe int)
        |> required "fullName" string
        |> required "displayName" string
        |> required "color" string


encodeProject : Project -> Json.Encode.Value
encodeProject x =
    Json.Encode.object
        [ ( "externalId", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.int) x.externalId )
        , ( "fullName", Json.Encode.string x.fullName )
        , ( "displayName", Json.Encode.string x.displayName )
        , ( "color", Json.Encode.string x.color )
        ]


type alias WorkOrder =
    { projectId : DataId
    , minutes : Int
    , startDate : Date
    , endDate : Date
    }


decodeWorkOrder : Decoder WorkOrder
decodeWorkOrder =
    decode WorkOrder
        |> required "projectId" decodeDataId
        |> required "minutes" int
        |> required "startDate" decodeDate
        |> required "endDate" decodeDate


encodeWorkOrder : WorkOrder -> Json.Encode.Value
encodeWorkOrder x =
    Json.Encode.object
        [ ( "projectId", encodeDataId x.projectId )
        , ( "minutes", Json.Encode.int x.minutes )
        , ( "startDate", (Json.Encode.string << toString) x.startDate )
        , ( "endDate", (Json.Encode.string << toString) x.endDate )
        ]


type alias Booking =
    { resourceId : DataId
    , bookDate : Date
    , projectId : DataId
    , amount : Int
    }


decodeBooking : Decoder Booking
decodeBooking =
    decode Booking
        |> required "resourceId" decodeDataId
        |> required "bookDate" decodeDate
        |> required "projectId" decodeDataId
        |> required "amount" int


encodeBooking : Booking -> Json.Encode.Value
encodeBooking x =
    Json.Encode.object
        [ ( "resourceId", encodeDataId x.resourceId )
        , ( "bookDate", (Json.Encode.string << toString) x.bookDate )
        , ( "projectId", encodeDataId x.projectId )
        , ( "amount", Json.Encode.int x.amount )
        ]


type alias PlanResRow =
    { resourceId : DataId
    , booked : List PlanResProjectRow
    , capacities : List ( Date, PlanResCapacity )
    }


decodePlanResRow : Decoder PlanResRow
decodePlanResRow =
    decode PlanResRow
        |> required "resourceId" decodeDataId
        |> required "booked" (list decodePlanResProjectRow)
        |> required "capacities" (list (map2 (,) (index 0 decodeDate) (index 1 decodePlanResCapacity)))


encodePlanResRow : PlanResRow -> Json.Encode.Value
encodePlanResRow x =
    Json.Encode.object
        [ ( "resourceId", encodeDataId x.resourceId )
        , ( "booked", (Json.Encode.list << List.map encodePlanResProjectRow) x.booked )
        , ( "capacities", (Json.Encode.list << List.map (tuple2 (Json.Encode.string << toString) encodePlanResCapacity)) x.capacities )
        ]


type alias PlanResProjectRow =
    { projectId : DataId
    , bookings : List PlanResBooking
    }


decodePlanResProjectRow : Decoder PlanResProjectRow
decodePlanResProjectRow =
    decode PlanResProjectRow
        |> required "projectId" decodeDataId
        |> required "bookings" (list decodePlanResBooking)


encodePlanResProjectRow : PlanResProjectRow -> Json.Encode.Value
encodePlanResProjectRow x =
    Json.Encode.object
        [ ( "projectId", encodeDataId x.projectId )
        , ( "bookings", (Json.Encode.list << List.map encodePlanResBooking) x.bookings )
        ]


type alias PlanResBooking =
    { bookingId : DataId
    , bookingDate : Date
    , bookingAmount : Int
    }


decodePlanResBooking : Decoder PlanResBooking
decodePlanResBooking =
    decode PlanResBooking
        |> required "bookingId" decodeDataId
        |> required "bookingDate" decodeDate
        |> required "bookingAmount" int


encodePlanResBooking : PlanResBooking -> Json.Encode.Value
encodePlanResBooking x =
    Json.Encode.object
        [ ( "bookingId", encodeDataId x.bookingId )
        , ( "bookingDate", (Json.Encode.string << toString) x.bookingDate )
        , ( "bookingAmount", Json.Encode.int x.bookingAmount )
        ]


getResources : String -> Http.Request (List ( DataId, Resource ))
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
            Http.expectJson (list (map2 (,) (index 0 decodeDataId) (index 1 decodeResource)))
        , timeout =
            Nothing
        , withCredentials =
            False
        }


getProjects : String -> Http.Request (List ( DataId, Project ))
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
            Http.expectJson (list (map2 (,) (index 0 decodeDataId) (index 1 decodeProject)))
        , timeout =
            Nothing
        , withCredentials =
            False
        }


postApiV1Projects : String -> Project -> Http.Request DataId
postApiV1Projects urlBase body =
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
                , "projects"
                ]
        , body =
            Http.jsonBody (encodeProject body)
        , expect =
            Http.expectJson decodeDataId
        , timeout =
            Nothing
        , withCredentials =
            False
        }


getApiV1Workorders : String -> Http.Request (List ( DataId, WorkOrder ))
getApiV1Workorders urlBase =
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
                , "workorders"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list (map2 (,) (index 0 decodeDataId) (index 1 decodeWorkOrder)))
        , timeout =
            Nothing
        , withCredentials =
            False
        }


postApiV1Workorders : String -> WorkOrder -> Http.Request DataId
postApiV1Workorders urlBase body =
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
            Http.jsonBody (encodeWorkOrder body)
        , expect =
            Http.expectJson decodeDataId
        , timeout =
            Nothing
        , withCredentials =
            False
        }


getApiV1BookingsByStartDateByEndDate : String -> String -> String -> Http.Request (List ( DataId, Booking ))
getApiV1BookingsByStartDateByEndDate urlBase capture_startDate capture_endDate =
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
            Http.expectJson (list (map2 (,) (index 0 decodeDataId) (index 1 decodeBooking)))
        , timeout =
            Nothing
        , withCredentials =
            False
        }


postApiV1BookingsByBookingId : String -> DataId -> Booking -> Http.Request ()
postApiV1BookingsByBookingId urlBase capture_bookingId body =
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
                    if String.isEmpty body then
                        Ok ()
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }


deleteApiV1BookingsByBookingId : String -> DataId -> Http.Request ()
deleteApiV1BookingsByBookingId urlBase capture_bookingId =
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
                    if String.isEmpty body then
                        Ok ()
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }


postApiV1Bookings : String -> Booking -> Http.Request DataId
postApiV1Bookings urlBase body =
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
            Http.expectJson decodeDataId
        , timeout =
            Nothing
        , withCredentials =
            False
        }


getApiV1PlanningResourcesByStartDateByEndDate : String -> String -> String -> Http.Request (List PlanResRow)
getApiV1PlanningResourcesByStartDateByEndDate urlBase capture_startDate capture_endDate =
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
                , "planning"
                , "resources"
                , capture_startDate |> Http.encodeUri
                , capture_endDate |> Http.encodeUri
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
