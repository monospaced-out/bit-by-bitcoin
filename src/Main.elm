module Main exposing (..)

import Html exposing (Html, button, text, div, h1, h2, br)
-- import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Sha256 exposing (sha256)
import Random exposing (generate, int)



---- MODEL ----


type BlockChain = BlockChain (List Block)

type alias Miner = { blockToErase : Maybe Block }

type alias Block = {
  transaction : Transaction,
  chain : BlockChain,
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
  blockChainOrigin : BlockChain,
  transactionPool : List Transaction,
  mainAddresses : List Address,
  randomValue : Int
}

txHash : Transaction -> String
txHash tx = sha256 (toString tx.amount ++ tx.sender.hash ++ tx.recipient.hash)

blockHash : Block -> String
blockHash block = sha256 (txHash block.transaction ++ block.nonce) -- needs more input to hash

minerDisplay : Miner -> String
minerDisplay miner =
  case miner.blockToErase of
    Nothing ->
      "just chillin"
    Just block ->
      "Trying to erase block: " ++ blockHash block

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

init : ( Model, Cmd Msg )
init =
  (
    {
      miners = [
        newMiner Nothing,
        newMiner Nothing,
        newMiner Nothing,
        newMiner Nothing,
        newMiner Nothing
      ],
      blockChainOrigin = BlockChain [],
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Next ->
      ( { model | randomValue = 1 }, Random.generate RandomEvent (Random.int 1 100000) )
    RandomEvent randomValue ->
      ({ model | randomValue = randomValue }, Cmd.none)


---- VIEW ----


view : Model -> Html Msg
view model = div []
  [
    h1 [] [ text "bit by bitcoin" ],
    button [ onClick Next ] [ text "Next" ],
    h2 [] [ text "Miners" ],
    model.miners
      |> List.concatMap ( \miner -> [
          text ("• " ++ minerDisplay miner),
          br [] []
        ] )
      |> div [],
    h2 [] [ text "Transaction Pool" ],
    model.transactionPool
      |> List.concatMap ( \tx -> [
          text ("• " ++ txHash tx),
          br [] []
        ] )
      |> div [],
    h2 [] [ text "Joe Schmo's Neighborhood" ],
    model.mainAddresses
      |> List.concatMap ( \address -> [
          text ("• " ++ address.hash ++ ": ? BTC"), -- fill in with computed BTC from blockchain
          br [] []
        ] )
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
