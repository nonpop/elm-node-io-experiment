module Main exposing (main)

import IO exposing (IO)
import IO.Ports


program : IO ()
program =
    IO.do IO.getArgs <|
        \args ->
            case args of
                [] ->
                    IO.do_ (IO.putStrLn "This is the Echoer; 'quit' quits.") <|
                        IO.do_ (IO.putStrLn "Also try running with command line args") <|
                            loop

                _ ->
                    echoAll args


loop : IO ()
loop =
    IO.do IO.getLine <|
        \line ->
            if line == "quit" then
                IO.return ()

            else
                IO.do_ (echoOne line) <|
                    loop


echoOne : String -> IO ()
echoOne line =
    IO.putStrLn ("(" ++ line ++ ")")


echoAll : List String -> IO ()
echoAll args =
    case args of
        [] ->
            IO.return ()

        first :: rest ->
            IO.do_ (echoOne first) <|
                echoAll rest


main : IO.Program
main =
    IO.run IO.Ports.ports program
