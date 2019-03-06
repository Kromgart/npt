module Messages exposing (Msg(..))

-- service modules

import Http
import Navigation exposing (Location)
import Date exposing (Date)


-- application modules

import Data.Shared
import Page.Resources as Resources
import Page.Projects as Projects
import Page.WorkOrders as WorkOrders
import Page.WorkOrdersExecutive as WorkOrdersExecutive


-- application message type


type Msg
    = NoOp
    | LocationChange Location
    | NotFoundLoad ()
    | Init Location (Result Http.Error Data.Shared.DataStorage)
    | ProjectsLoad (Result Http.Error Projects.Model)
    | WorkOrdersLoad (Result Http.Error WorkOrders.Model)
    | ResourcesMsg Resources.Msg
    | ProjectsMsg Projects.Msg
    | WorkOrdersMsg WorkOrders.Msg
    | WorkOrdersExecutiveMsg WorkOrdersExecutive.Msg
