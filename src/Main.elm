module Main exposing (..)

import Html exposing (Html, button, text, div, h1, h2, br)
-- import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Sha256 exposing (sha256)
import Time exposing (now)



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
txHash tx = sha256 (toString (.amount tx) ++ .hash (.sender tx) ++ .hash (.recipient tx))

blockHash : Block -> String
blockHash block = sha256 (txHash (.transaction block) ++ .nonce block) -- needs more input to hash

minerDisplay : Miner -> String
minerDisplay miner =
  case (.blockToErase miner) of
    Nothing ->
      "just chillin"
    Just block ->
      "Trying to erase block: " ++ blockHash block

newMiner : Miner
newMiner = { blockToErase = Nothing }

newAddress : Address
newAddress = { hash = sha256 (toString Time.now) }

newTransaction : Transaction
newTransaction = { sender = newAddress, recipient = newAddress, amount = 1 }

init : ( Model, Cmd Msg )
init =
  (
    {
      miners = [
        newMiner,
        newMiner,
        newMiner,
        newMiner,
        newMiner
      ],
      blockChainOrigin = BlockChain [],
      transactionPool = [
        newTransaction,
        newTransaction,
        newTransaction,
        newTransaction,
        newTransaction,
        newTransaction,
        newTransaction,
        newTransaction,
        newTransaction,
        newTransaction
      ],
      mainAddresses = [
        newAddress,
        newAddress,
        newAddress,
        newAddress,
        newAddress
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
    .miners model
      |> List.concatMap ( \miner -> [
          text ("• " ++ minerDisplay miner),
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
