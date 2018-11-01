module IO exposing
    ( IO
    , JsonValue
    , Ports
    , Program
    , do
    , do_
    , getArgs
    , getLine
    , putStrLn
    , return
    , run
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Task


type IO a
    = IO ((a -> Action) -> Action)


return : a -> IO a
return value =
    IO (\k -> k value)


do : IO a -> (a -> IO b) -> IO b
do (IO c) f =
    IO (\k -> c (\a -> unwrap (f a) k))


unwrap : IO a -> (a -> Action) -> Action
unwrap (IO c) =
    c


type Action
    = Stop
    | GetArgs (List String -> Action)
    | GetLine (String -> Action)
    | PutStrLn String (() -> Action)


getArgs : IO (List String)
getArgs =
    IO GetArgs


getLine : IO String
getLine =
    IO GetLine


putStrLn : String -> IO ()
putStrLn str =
    IO (PutStrLn str)


do_ : IO () -> IO b -> IO b
do_ io1 io2 =
    do io1 (\() -> io2)


type alias JsonValue =
    Decode.Value


type alias Flags =
    {}


type alias Model =
    Action


type Msg
    = Perform
    | Result JsonValue


step : Cmd Msg
step =
    Task.perform (\() -> Perform) (Task.succeed ())


init : IO () -> Flags -> ( Model, Cmd Msg )
init program flags =
    ( unwrap program (\() -> Stop), step )


type alias Ports =
    { perform : { action : String, arg : JsonValue } -> Cmd Msg
    , result : (JsonValue -> Msg) -> Sub Msg
    }


subscriptions : Ports -> Model -> Sub Msg
subscriptions ports model =
    case model of
        Stop ->
            Sub.none

        _ ->
            ports.result Result


update : Ports -> Msg -> Model -> ( Model, Cmd Msg )
update ports msg model =
    case ( model, msg ) of
        ( Stop, _ ) ->
            ( model, Cmd.none )

        ( GetArgs next, Perform ) ->
            ( model, ports.perform { action = "getArgs", arg = Encode.null } )

        ( GetArgs next, Result res ) ->
            decodeResult ports res (Decode.list Decode.string) next

        ( GetLine next, Perform ) ->
            ( model, ports.perform { action = "getLine", arg = Encode.null } )

        ( GetLine next, Result res ) ->
            decodeResult ports res Decode.string next

        ( PutStrLn arg next, Perform ) ->
            ( model, ports.perform { action = "putStrLn", arg = Encode.string arg } )

        ( PutStrLn arg next, Result res ) ->
            ( next (), step )


decodeResult : Ports -> JsonValue -> Decoder a -> (a -> Model) -> ( Model, Cmd Msg )
decodeResult ports resVal decoder next =
    case Decode.decodeValue decoder resVal of
        Ok res ->
            ( next res, step )

        Err err ->
            ( Stop
            , ports.perform
                { action = "putStrLn"
                , arg = Encode.string ("Decode error: " ++ Decode.errorToString err)
                }
            )


type alias Program =
    Platform.Program Flags Model Msg


run : Ports -> IO () -> Program
run ports io =
    Platform.worker
        { init = init io
        , update = update ports
        , subscriptions = subscriptions ports
        }
