module Main exposing (..)

import Html exposing (Html, Attribute, button, text, div, h1, h2, br)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Sha256 exposing (sha256)
import Random
import String exposing (slice)
import List exposing (head, concatMap, indexedMap, filter, isEmpty, drop, append, range, map, take, length)


numMainAddresses = 5
lastMainAddressIndex = numMainAddresses - 1

---- MODEL ----


type alias Miner = { blockToErase : Maybe Block }

type BlockLink = BlockLink Block | OriginBlock

type alias Block = {
  transaction : Transaction,
  previousBlock : BlockLink,
  nonce : String,
  hashCache : String
}

type alias Transaction = {
  sender : Address,
  recipient : Address,
  amount : Int
}

type alias Address = {
  hash : String,
  balance : Int
}

type alias Model = {
  miners : List Miner,
  discoveredBlocks : List BlockLink,
  transactionPool : List Transaction,
  addressBook : List Address,
  randomValue : Int
}

txHash : Transaction -> String
txHash tx =
  toString tx.amount ++ tx.sender.hash ++ tx.recipient.hash
  |> sha256

blockHash : Block -> String
blockHash block = block.hashCache

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
      case head <| model.discoveredBlocks of
        Nothing ->
          ""
        Just block ->
          ". trying nonce " ++ chooseNonce minerIndex model.randomValue ++ ": " ++
            hashDisplay (testBlockHash transaction block minerIndex model.randomValue)

blockDisplay : BlockLink -> String
blockDisplay blocklink =
  "• " ++ hashDisplay (blockLinkHash blocklink) ++
  case blocklink of
    OriginBlock ->
      " (origin block)"
    BlockLink block ->
      " {transaction: " ++ hashDisplay (txHash block.transaction) ++
        ", nonce: " ++ block.nonce ++ ", previousBlock: " ++
        hashDisplay (blockLinkHash block.previousBlock) ++ "}"

txDisplay : Transaction -> String
txDisplay tx =
  "• " ++ hashDisplay (txHash tx) ++ " {from: " ++ hashDisplay tx.sender.hash ++
    ", to: " ++ hashDisplay tx.recipient.hash ++ ", amount: " ++
    toString tx.amount ++ "BTC}"

hashDisplay : String -> String
hashDisplay hash = slice 0 10 hash ++ "..."

newMiner : Maybe Block -> Miner
newMiner block = { blockToErase = block }

newAddress : Int -> Int -> Address
newAddress seed balance = { hash = sha256 (toString seed), balance = balance }

newTx : Int -> Int -> Int -> Transaction
newTx senderSeed recipientSeed amount =
  {
    sender = newAddress senderSeed 10,
    recipient = newAddress recipientSeed 10,
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
      miners = range 0 19
        |> map (\n -> newMiner Nothing),
      discoveredBlocks = [OriginBlock],
      transactionPool = range 0 (lastMainAddressIndex + 5)
        |> map (\i -> newTx (2 * i + numMainAddresses) (2 * i + numMainAddresses + 1) 1),
      addressBook = range 0 (lastMainAddressIndex + 20)
        |> map (\i -> newAddress i 10),
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
      case head <| model.discoveredBlocks of
        Nothing ->
          model
        Just block ->
          let
            results = model.miners
              |> indexedMap (
                \m miner -> (
                  (chooseNonce m model.randomValue),
                  (testBlockHash transaction block m model.randomValue)
                )
              )
              |> filter (\(nonce, hash) -> (slice 0 2 hash) == "00")
            newTxSender = length model.addressBook
            newTxRecipient = length model.addressBook + 1
            newTxAmount = 1
            newAddressBalance = 10
            removeMinedTx = (drop 1 model.transactionPool)
            replacementTx = newTx newTxSender newTxRecipient newTxAmount
          in
            case head results of
              Nothing ->
                model
              Just (nonce, hash) ->
                { model |
                  discoveredBlocks = BlockLink {
                    transaction = transaction,
                    previousBlock = block,
                    nonce = nonce,
                    hashCache = hash
                  } :: model.discoveredBlocks,
                  transactionPool = append removeMinedTx [replacementTx],
                  addressBook = append model.addressBook [
                    (newAddress newTxSender newAddressBalance),
                    (newAddress newTxRecipient newAddressBalance)
                  ]
                }

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
      case head <| model.discoveredBlocks of
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
    h2 [] [ text "Mined Blocks" ],
    model.discoveredBlocks
      |> concatMap ( \blocklink -> [
          text (blockDisplay blocklink),
          br [] []
        ] )
      |> div [],
    h2 [] [ text "Transaction Pool" ],
    model.transactionPool
      |> concatMap ( \tx -> [
          text (txDisplay tx),
          br [] []
        ] )
      |> div [],
    h2 [] [ text "Joe Schmo's Neighborhood" ],
    take numMainAddresses model.addressBook
      |> concatMap ( \address -> [
          text ("• " ++ hashDisplay address.hash ++ " " ++ toString address.balance ++ " BTC"), -- fill in with computed BTC from blockchain
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
