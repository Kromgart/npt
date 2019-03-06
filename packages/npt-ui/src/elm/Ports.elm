port module Ports exposing (..)

import Json.Encode exposing (Value)


-- Commands


port datepickerData : Maybe Value -> Cmd msg


port monthpickerData : Maybe Value -> Cmd msg


port beforePageLeave : () -> Cmd msg


port pageLoad : () -> Cmd msg



-- Subscriptions


port onDatepickerPlanResChange : (Value -> msg) -> Sub msg


port onDatepickerWOChange : (Value -> msg) -> Sub msg


port onDatepickerWOExecChange : (Value -> msg) -> Sub msg


port onResTableScroll : (() -> msg) -> Sub msg


port onClick : (Value -> msg) -> Sub msg



-- port pushState : ( String, String ) -> Cmd msg
-- port onPopState : (Value -> msg) -> Sub msg
