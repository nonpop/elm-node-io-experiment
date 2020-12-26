module IO exposing
    ( IO
    , JsonValue
    , Ports
    , Program
    , do
    , do_
    , getArgs
    , getLine
    , lazy
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


fail : String -> IO a
fail err =
    IO (\_ -> Stop (Just err))


do : IO a -> (a -> IO b) -> IO b
do (IO c) f =
    IO (\k -> c (\a -> unwrap (f a) k))


unwrap : IO a -> (a -> Action) -> Action
unwrap (IO c) =
    c


lazy : (() -> IO a) -> IO a
lazy mkIO =
    IO (\k -> unwrap (mkIO ()) k)


type Action
    = Stop (Maybe String)
    | Next String Encode.Value (Decode.Value -> Action)


decodeResult : Decoder a -> Decode.Value -> IO a
decodeResult decoder value =
    case Decode.decodeValue decoder value of
        Ok res ->
            return res

        Err err ->
            fail ("Decode error: " ++ Decode.errorToString err)


call : String -> Encode.Value -> Decoder a -> IO a
call action arg resultDecoder =
    do (IO (Next action arg)) <| decodeResult resultDecoder


call_ : String -> Encode.Value -> IO ()
call_ action arg =
    do (IO (Next action arg)) <| decodeResult (Decode.succeed ())


getArgs : IO (List String)
getArgs =
    call "getArgs" Encode.null (Decode.list Decode.string)


getLine : IO String
getLine =
    call "getLine" Encode.null Decode.string


putStrLn : String -> IO ()
putStrLn str =
    call_ "putStrLn" (Encode.string str)


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
init program {} =
    ( unwrap program (\() -> Stop Nothing), step )


type alias Ports =
    { perform : { action : String, arg : JsonValue } -> Cmd Msg
    , result : (JsonValue -> Msg) -> Sub Msg
    }


subscriptions : Ports -> Model -> Sub Msg
subscriptions ports model =
    case model of
        Stop _ ->
            Sub.none

        _ ->
            ports.result Result


update : Ports -> Msg -> Model -> ( Model, Cmd Msg )
update ports msg model =
    case ( model, msg ) of
        ( Stop Nothing, _ ) ->
            ( model, Cmd.none )

        ( Stop (Just err), _ ) ->
            ( Stop Nothing
            , ports.perform
                { action = "putStrLn"
                , arg = Encode.string ("Error: " ++ err)
                }
            )

        ( Next action arg next, Perform ) ->
            ( model, ports.perform { action = action, arg = arg } )

        ( Next action arg next, Result res ) ->
            ( next res, step )


type alias Program =
    Platform.Program Flags Model Msg


run : Ports -> IO () -> Program
run ports io =
    Platform.worker
        { init = init io
        , update = update ports
        , subscriptions = subscriptions ports
        }
