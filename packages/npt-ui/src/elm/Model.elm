module Model exposing (Model, Page(..), PageState(..), getData)

import Task exposing (Task)
import Http
import Navigation exposing (Location)
import List.Extra


-- application modules

import Data.Shared exposing (DataStorage, Resource, ResourceID, Project, ProjectID, ProjectType, ProjectTypeID)
import Request.Shared exposing (serverDomain, getResources, getProjects, getProjectTypes, getGroups)
import Page.Resources as Resources
import Page.Projects as Projects
import Page.WorkOrders as WorkOrders
import Page.WorkOrdersExecutive as WorkOrdersExecutive
import Page.Errored as Errored


-- defining page state


type Page
    = Resources Resources.Model
    | Projects Projects.Model
    | WorkOrders WorkOrders.Model
    | WorkOrdersExecutive WorkOrdersExecutive.Model
    | Errored Errored.Model
    | NotFound
    | Blank


type PageState
    = Loaded Page
    | TransitioningFrom Page



-- application global model


type alias Model =
    { pageState : PageState
    , dataStorage : DataStorage
    }



-- get data storage


getData : Task Http.Error DataStorage
getData =
    let
        fetchResourcesTask =
            getResources serverDomain
                |> Http.toTask

        fetchProjectsTask =
            getProjects serverDomain
                |> Http.toTask

        fetchProjectTypesTask =
            getProjectTypes serverDomain
                |> Http.toTask

        fetchGroupsTask =
            getGroups serverDomain
                |> Http.toTask
    in
        Task.map4 DataStorage fetchResourcesTask fetchProjectsTask fetchProjectTypesTask fetchGroupsTask
