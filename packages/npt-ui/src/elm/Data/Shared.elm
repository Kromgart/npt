module Data.Shared exposing (..)

import Exts.Json.Decode exposing (decodeDate)
import Date exposing (Date)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode


-- storing persistent data


type alias DataStorage =
    { resources : List ( ResourceID, Resource )
    , projects : List ( ProjectID, Project )
    , projectTypes : List ( ProjectTypeID, ProjectType )
    , groups : List ( GroupID, Group )
    }


type alias DateRange =
    { startDate : Date
    , endDate : Date
    }


decodeDateRange : Decoder DateRange
decodeDateRange =
    decode DateRange
        |> required "startDate" decodeDate
        |> required "endDate" decodeDate


encodeDateRange : DateRange -> Json.Encode.Value
encodeDateRange x =
    Json.Encode.object
        [ ( "startDate", (Json.Encode.string << toString) x.startDate )
        , ( "endDate", (Json.Encode.string << toString) x.endDate )
        ]


type EditableData a b
    = Empty a
    | Actual (DataID b) b b
    | New b
    | Creating b
    | Updating (DataID b) b b
    | Deleting (DataID b) b b
    | Error String (EditableData a b)


type DataID a
    = DataID Int


extractValue : DataID b -> EditableData a b -> Maybe b
extractValue id eData =
    case eData of
        Actual _ rec _ ->
            Just rec

        _ ->
            Nothing



-- Resource data structure


type alias ResourceID =
    DataID Resource


decodeResourceID : Decoder ResourceID
decodeResourceID =
    andThen (succeed << DataID) int


encodeResourceID : ResourceID -> Json.Encode.Value
encodeResourceID (DataID a) =
    Json.Encode.int a


type alias Resource =
    { externalId : Maybe Int
    , supervisor : Maybe Int
    , name : String
    , schedule : Int
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
        |> required "supervisor" (maybe int)
        |> required "name" string
        |> required "schedule" int
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
        , ( "supervisor", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.int) x.supervisor )
        , ( "name", Json.Encode.string x.name )
        , ( "schedule", Json.Encode.int x.schedule )
        , ( "startDate", (Json.Encode.string << toString) x.startDate )
        , ( "endDate", (Maybe.withDefault Json.Encode.null << Maybe.map (Json.Encode.string << toString)) x.endDate )
        , ( "title", Json.Encode.string x.title )
        , ( "groups", (Json.Encode.list << List.map Json.Encode.int) x.groups )
        , ( "email", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.email )
        , ( "phone", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.phone )
        ]



-- Project data structure


type alias ProjectID =
    DataID Project


decodeProjectID : Decoder ProjectID
decodeProjectID =
    andThen (succeed << DataID) int


encodeProjectID : ProjectID -> Json.Encode.Value
encodeProjectID (DataID a) =
    Json.Encode.int a


type alias Project =
    { externalId : Maybe Int
    , fullName : String
    , displayName : String
    , color : String
    , accountMgr : Maybe ResourceID
    , projectMgr : Maybe ResourceID
    , projectType : ProjectTypeID
    }


decodeProject : Decoder Project
decodeProject =
    decode Project
        |> required "externalId" (maybe int)
        |> required "fullName" string
        |> required "displayName" string
        |> required "color" string
        |> required "accountMgr" (maybe decodeResourceID)
        |> required "projectMgr" (maybe decodeResourceID)
        |> required "projectType" decodeProjectTypeID


encodeProject : Project -> Json.Encode.Value
encodeProject x =
    Json.Encode.object
        [ ( "externalId", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.int) x.externalId )
        , ( "fullName", Json.Encode.string x.fullName )
        , ( "displayName", Json.Encode.string x.displayName )
        , ( "color", Json.Encode.string x.color )
        , ( "accountMgr", (Maybe.withDefault Json.Encode.null << Maybe.map encodeResourceID) x.accountMgr )
        , ( "projectMgr", (Maybe.withDefault Json.Encode.null << Maybe.map encodeResourceID) x.projectMgr )
        , ( "projectType", encodeProjectTypeID x.projectType )
        ]



-- Project type data structure


type alias ProjectTypeID =
    DataID ProjectType


decodeProjectTypeID : Decoder ProjectTypeID
decodeProjectTypeID =
    andThen (succeed << DataID) int


encodeProjectTypeID : ProjectTypeID -> Json.Encode.Value
encodeProjectTypeID (DataID a) =
    Json.Encode.int a


type alias ProjectType =
    { displayName : String }


decodeProjectType : Decoder ProjectType
decodeProjectType =
    decode ProjectType
        |> required "displayName" string



-- Group type data structure


type alias GroupID =
    DataID Group


decodeGroupID : Decoder GroupID
decodeGroupID =
    andThen (succeed << DataID) int


encodeGroupID : GroupID -> Json.Encode.Value
encodeGroupID (DataID a) =
    Json.Encode.int a


type alias Group =
    { displayName : String
    , groupTypeId : Int
    }


decodeGroup : Decoder Group
decodeGroup =
    decode Group
        |> required "displayName" string
        |> required "groupTypeId" int



-- WorkOrder data structure


type alias WorkOrderID =
    DataID WorkOrder


decodeWorkOrderID : Decoder WorkOrderID
decodeWorkOrderID =
    andThen (succeed << DataID) int


encodeWorkOrderID : WorkOrderID -> Json.Encode.Value
encodeWorkOrderID (DataID a) =
    Json.Encode.int a


type alias WorkOrder =
    { projectId : ProjectID
    , budget : Int
    , extraBudget : Int
    , nonBillable : Int
    , startDate : Date
    , endDate : Date
    , isApproved : Bool
    }


decodeWorkOrder : Decoder WorkOrder
decodeWorkOrder =
    decode WorkOrder
        |> required "projectId" decodeProjectID
        |> required "budget" int
        |> required "extraBudget" int
        |> required "nonBillable" int
        |> required "startDate" decodeDate
        |> required "endDate" decodeDate
        |> required "isApproved" bool


encodeWorkOrder : WorkOrder -> Json.Encode.Value
encodeWorkOrder x =
    Json.Encode.object
        [ ( "projectId", encodeProjectID x.projectId )
        , ( "budget", Json.Encode.int x.budget )
        , ( "extraBudget", Json.Encode.int x.extraBudget )
        , ( "nonBillable", Json.Encode.int x.nonBillable )
        , ( "startDate", (Json.Encode.string << toString) x.startDate )
        , ( "endDate", (Json.Encode.string << toString) x.endDate )
        , ( "isApproved", Json.Encode.bool x.isApproved )
        ]
