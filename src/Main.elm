module Main exposing (..)

import Settings exposing (numMainAddresses, lastMainAddressIndex, numMiners)
import Model exposing (Model, Msg(ProvideNames, RandomEvent), BlockLink(NoBlock), Flags, newMiner, newTx, newAddress)
import Update exposing (update)
import View exposing (view)
import Array exposing (fromList, get)
import List exposing (map, range)
import Html exposing (programWithFlags)
import Ports exposing (provideNames)
import Random

init : Flags -> ( Model, Cmd Msg )
init flags =
  (
    let
      namesList = flags.initialNames
      namesArray = fromList namesList
      firstNameInList = case get 0 namesArray of
        Nothing -> ""
        Just name -> name
      secondNameInList = case get 1 namesArray of
        Nothing -> ""
        Just name -> name
    in
      {
        miners = range 0 (numMiners - 1)
          |> map (\n -> newMiner NoBlock),
        discoveredBlocks = [ NoBlock ],
        transactionPool = range 0 3
          |> map (\i ->
            let
              senderIndex = 2 * i + numMainAddresses
              receiverIndex = 2 * i + numMainAddresses + 1
              senderName = case get senderIndex namesArray of
                Nothing -> "Sender"
                Just name -> name
              receiverName = case get receiverIndex namesArray of
                Nothing -> "Receiver"
                Just name -> name
            in
              newTx (senderName, senderIndex) (receiverName, receiverIndex) 1
            ),
        addressBook = range 0 (lastMainAddressIndex + 8)
          |> map (\i ->
            case get i namesArray of
              Nothing -> newAddress "N/A" i 10
              Just name -> newAddress name i 10
          ),
        randomValue = 0,
        randomNames = namesList,
        txForm = {
          sender = (newAddress firstNameInList 0 10).hash,
          receiver = (newAddress secondNameInList 0 10).hash,
          amount = 0
        }
      },
    Random.generate RandomEvent (Random.int 1 100000)
  )

subscriptions : Model -> Sub Msg
subscriptions model =
  provideNames ProvideNames

main : Program Flags Model Msg
main = programWithFlags
  {
    view = view,
    init = init,
    update = update,
    subscriptions = subscriptions
  }
