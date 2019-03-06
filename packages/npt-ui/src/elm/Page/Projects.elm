module Page.Projects exposing (..)

-- Service modules

import Http
import Task exposing (Task)
import Date exposing (Date)


-- Application modules

import Data.Shared exposing (Project, ProjectID)
import Request.Shared exposing (serverDomain, getProjects)


-- Define projects page model


type alias Model =
    { planningFeed : List ( ProjectID, Project )
    }



-- Get projects page model


get : Result String Date -> Result String Date -> Task Http.Error Model
get startDate endDate =
    getProjects serverDomain
        |> Http.toTask
        |> Task.map Model


type Msg
    = NoOp
    | LoadPage (Result Http.Error Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LoadPage (Ok newModel) ->
            ( newModel, Cmd.none )

        LoadPage (Err _) ->
            ( model, Cmd.none )
