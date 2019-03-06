port module Module.Collapsible.Ports exposing (..)

import Json.Encode exposing (Value)


-- Commands


port collapsibleCollapse : Int -> Cmd msg


port collapsibleExpand : Int -> Cmd msg
