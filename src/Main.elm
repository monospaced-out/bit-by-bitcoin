module Main exposing (..)

import Settings exposing (numMainAddresses, lastMainAddressIndex, numMiners)
import Model exposing (Model, Msg(RandomEvent), BlockLink(NoBlock), newMiner, newTx, newAddress)
import Update exposing (update)
import View exposing (view)
import List exposing (map, range)
import Html exposing (Html)
import Random

init : ( Model, Cmd Msg )
init =
  (
    {
      miners = range 0 (numMiners - 1)
        |> map (\n -> newMiner NoBlock),
      discoveredBlocks = [ NoBlock ],
      transactionPool = range 0 3
        |> map (\i -> newTx (2 * i + numMainAddresses) (2 * i + numMainAddresses + 1) 1),
      addressBook = range 0 (lastMainAddressIndex + 8)
        |> map (\i -> newAddress i 10),
      randomValue = 0,
      txForm = {
        sender = (newAddress 0 10).hash,
        receiver = (newAddress 0 10).hash,
        amount = 0
      }
    },
    Random.generate RandomEvent (Random.int 1 100000)
  )

main : Program Never Model Msg
main = Html.program
  {
    view = view,
    init = init,
    update = update,
    subscriptions = always Sub.none
  }
