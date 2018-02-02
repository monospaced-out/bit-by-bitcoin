port module Ports exposing (..)

port getNames : Int -> Cmd msg
port provideNames : (List String -> msg) -> Sub msg
