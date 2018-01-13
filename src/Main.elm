module Main exposing (..)

import Html exposing (Html, button, text, div, h1, h2, br)
-- import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Sha256 exposing (sha256)



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
  mainAddresses : List Address
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
        newTx 01 02 1,
        newTx 11 12 1,
        newTx 21 22 1,
        newTx 31 32 1,
        newTx 41 42 1,
        newTx 51 52 1,
        newTx 61 62 1,
        newTx 71 72 1,
        newTx 81 82 1,
        newTx 91 92 1
      ],
      mainAddresses = [
        newAddress 1,
        newAddress 2,
        newAddress 3,
        newAddress 4,
        newAddress 5
      ]
    },
    Cmd.none
  )



---- UPDATE ----


type Msg = Next


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  (
    case msg of
      Next ->
        model
    ,
    Cmd.none
  )



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
