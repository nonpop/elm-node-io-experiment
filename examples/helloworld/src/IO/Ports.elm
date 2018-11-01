port module IO.Ports exposing (ports)

import IO


port ioPerform : { action : String, arg : IO.JsonValue } -> Cmd msg


port ioResult : (IO.JsonValue -> msg) -> Sub msg


ports : IO.Ports
ports =
    { perform = ioPerform
    , result = ioResult
    }
