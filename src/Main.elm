module Main exposing (..)

import Html exposing (Html, Attribute, button, text, div, h1, h2, br)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Sha256 exposing (sha256)
import Random
import String exposing (slice)
import List exposing (head, reverse, map, concatMap, indexedMap, filter, isEmpty)



---- MODEL ----


type alias Miner = { blockToErase : Maybe Block }

type BlockLink = BlockLink Block | OriginBlock

type alias Block = {
  transaction : Transaction,
  previousBlock : BlockLink,
  nonce : String
}

type alias Transaction = {
  sender : Address,
  recipient : Address,
  amount : Int
}

type alias Address = {
  hash : String
}

type alias Model = {
  miners : List Miner,
  discoveredBlocks : List BlockLink,
  transactionPool : List Transaction,
  mainAddresses : List Address,
  randomValue : Int
}

txHash : Transaction -> String
txHash tx =
  toString tx.amount ++ tx.sender.hash ++ tx.recipient.hash
  |> sha256

blockHash : Block -> String
blockHash block =
  txHash block.transaction ++ blockLinkHash block.previousBlock ++ block.nonce
  |> sha256

blockLinkHash : BlockLink -> String
blockLinkHash blocklink =
  case blocklink of
    OriginBlock ->
      sha256 "0"
    BlockLink block ->
      blockHash block

minerDisplay : Miner -> String
minerDisplay miner =
  case miner.blockToErase of
    Nothing ->
      "• " ++ "just chillin"
    Just block ->
      "• " ++ "Trying to erase block: " ++ blockHash block

minerActionDisplay : Model -> Int -> String
minerActionDisplay model minerIndex =
  case head model.transactionPool of
    Nothing ->
      ""
    Just transaction ->
      case head <| reverse <| model.discoveredBlocks of
        Nothing ->
          ""
        Just block ->
          ". trying nonce " ++ chooseNonce minerIndex model.randomValue ++ ": " ++
            slice 0 10 (testBlockHash transaction block minerIndex model.randomValue) ++ "..."

blockDisplay : BlockLink -> String
blockDisplay blocklink =
  slice 0 10 (blockLinkHash blocklink) ++ "..."

newMiner : Maybe Block -> Miner
newMiner block = { blockToErase = block }

newAddress : Int -> Address
newAddress seed = { hash = sha256 (toString seed) }

newTx : Int -> Int -> Int -> Transaction
newTx senderSeed recipientSeed amount =
  {
    sender = newAddress senderSeed,
    recipient = newAddress recipientSeed,
    amount = amount
  }

chooseNonce : Int -> Int -> String
chooseNonce minerIndex seed = minerIndex + seed
  |> toString
  |> sha256
  |> slice 0 5

testBlockHash : Transaction -> BlockLink -> Int -> Int -> String
testBlockHash tx previousBlock minerIndex seed =
 txHash tx ++ blockLinkHash previousBlock ++ chooseNonce minerIndex seed
 |> sha256

init : ( Model, Cmd Msg )
init =
  (
    {
      miners = [
        newMiner Nothing,
        newMiner Nothing,
        newMiner Nothing,
        newMiner Nothing,
        newMiner Nothing,
        newMiner Nothing,
        newMiner Nothing,
        newMiner Nothing,
        newMiner Nothing,
        newMiner Nothing
      ],
      discoveredBlocks = [OriginBlock],
      transactionPool = [
        newTx 11 12 1,
        newTx 13 14 1,
        newTx 15 16 1,
        newTx 17 18 1,
        newTx 19 20 1,
        newTx 21 22 1,
        newTx 23 24 1,
        newTx 25 26 1,
        newTx 27 28 1,
        newTx 29 30 1
      ],
      mainAddresses = [
        newAddress 1,
        newAddress 2,
        newAddress 3,
        newAddress 4,
        newAddress 5
      ],
      randomValue = 0
    },
    Random.generate RandomEvent (Random.int 1 100000)
  )



---- UPDATE ----


type Msg = Next | RandomEvent Int

mine : Model -> Model
mine model =
  case head model.transactionPool of
    Nothing ->
      model
    Just transaction ->
      case head <| reverse <| model.discoveredBlocks of
        Nothing ->
          model
        Just block ->
          let
            results = model.miners
              |> indexedMap (
                \m miner -> ((chooseNonce m model.randomValue), (testBlockHash transaction block m model.randomValue))
              )
              |> filter (\(nonce, hash) -> (slice 0 2 hash) == "00")
          in
            if not (isEmpty results)
            then
              case head results of
                Nothing ->
                  model
                Just (nonce, hash) ->
                  { model | discoveredBlocks = BlockLink { transaction = transaction, previousBlock = block, nonce = nonce } :: model.discoveredBlocks }
            else
              model

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Next ->
      ( mine model, Random.generate RandomEvent (Random.int 1 100000) )
    RandomEvent randomValue ->
      ({ model | randomValue = randomValue }, Cmd.none)


---- VIEW ----


minerStyle : Model -> Int -> Attribute msg
minerStyle model minerIndex =
  case head model.transactionPool of
    Nothing ->
      style []
    Just transaction ->
      case head <| reverse <| model.discoveredBlocks of
        Nothing ->
          style []
        Just block ->
          if (slice 0 2 (testBlockHash transaction block minerIndex model.randomValue)) == "00"
            then style [ ("backgroundColor", "green") , ("color", "white") ]
            else style []

view : Model -> Html Msg
view model = div []
  [
    h1 [] [ text "bit by bitcoin" ],
    button [ onClick Next ] [ text "Next" ],
    h2 [] [ text "Miners" ],
    model.miners
      |> indexedMap ( \m miner ->
          div [ minerStyle model m ] [
            minerDisplay miner |> text,
            minerActionDisplay model m |> text,
            br [] []
          ]
        )
      |> div [],
    h2 [] [ text "Transaction Pool" ],
    model.transactionPool
      |> concatMap ( \tx -> [
          text ("• " ++ txHash tx),
          br [] []
        ] )
      |> div [],
    h2 [] [ text "Joe Schmo's Neighborhood" ],
    model.mainAddresses
      |> concatMap ( \address -> [
          text ("• " ++ address.hash ++ ": ? BTC"), -- fill in with computed BTC from blockchain
          br [] []
        ] )
      |> div [],
    h2 [] [ text "Mined Blocks" ],
    model.discoveredBlocks
      |> map ( \blocklink ->
          text (blockDisplay blocklink)
        )
      |> div []
  ]



---- PROGRAM ----


main : Program Never Model Msg
main = Html.program
  {
    view = view,
    init = init,
    update = update,
    subscriptions = always Sub.none
  }
