module View exposing (..)

-- service modules

import Html exposing (Html, map, div, header, text, h1, ul, li, a, img, nav)
import Html.Attributes exposing (src, class, classList)
import Html.Events exposing (onClick)


-- application modules

import Model exposing (Model, Page(..), PageState(..))
import Messages exposing (Msg(..))
import Router exposing (..)
import View.Resources as Resources
import View.Projects as Projects
import View.WorkOrders as WorkOrders
import View.WorkOrdersExecutive as WorkOrdersExecutive
import View.NotFound as NotFound
import View.Errored as Errored
import Assets exposing (..)
import Utils exposing (..)


getPage : PageState -> Page
getPage pageState =
    case pageState of
        Loaded page ->
            page

        TransitioningFrom page ->
            page


view : Model -> Html Msg
view model =
    viewPage model


viewPage : Model -> Html Msg
viewPage model =
    let
        page =
            getPage model.pageState
    in
        case page of
            Blank ->
                nothing

            Resources subModel ->
                Resources.view subModel model.dataStorage
                    |> frame page
                    |> Html.map ResourcesMsg

            Projects subModel ->
                Projects.view subModel
                    |> frame page
                    |> Html.map ResourcesMsg

            WorkOrders subModel ->
                WorkOrders.view subModel model.dataStorage
                    |> frame page
                    |> Html.map WorkOrdersMsg

            WorkOrdersExecutive subModel ->
                WorkOrdersExecutive.view subModel model.dataStorage
                    |> frame page
                    |> Html.map WorkOrdersExecutiveMsg

            Errored subModel ->
                Errored.view subModel
                    |> frame page
                    |> Html.map ResourcesMsg

            NotFound ->
                NotFound.view
                    |> frame page
                    |> Html.map ResourcesMsg


frame : Page -> Html msg -> Html msg
frame page content =
    div [ class "main-wrapper" ]
        [ headerView page
        , content
        ]


headerView : Page -> Html msg
headerView page =
    header [ class "site-header" ]
        [ h1 [ class "site-logo" ] [ img [ src <| assetUrl logo ] [] ]
        , navigationView page
        ]


navigationView : Page -> Html msg
navigationView page =
    nav [ class "site-main-nav" ]
        [ ul []
            [ navigationLink
                (case page of
                    Resources _ ->
                        True

                    _ ->
                        False
                )
                ResourcesRouteInit
                "People"
            , navigationLink
                (case page of
                    Projects _ ->
                        True

                    _ ->
                        False
                )
                ProjectsRoute
                "Projects"
            , navigationLink
                (case page of
                    WorkOrders _ ->
                        True

                    _ ->
                        False
                )
                WorkOrdersRouteInit
                "Work Orders"
            , navigationLink
                (case page of
                    WorkOrdersExecutive _ ->
                        True

                    _ ->
                        False
                )
                WorkOrdersExecutiveRouteInit
                "Work Orders Executive"
            ]
        ]


navigationLink : Bool -> Route -> String -> Html msg
navigationLink isActive route linkText =
    li []
        [ a
            [ classList [ ( "site-main-nav-active", isActive ) ]
            , href route
            ]
            [ text linkText ]
        ]
