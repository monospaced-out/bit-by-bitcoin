module Main exposing (..)

import Html exposing (Html, Attribute, button, text, div, h1, h2, br, form, input, select, option)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (on, onClick, onSubmit, onInput)
import Sha256 exposing (sha256)
import Random
import String exposing (slice, toInt)
import List exposing (head, concatMap, indexedMap, filter, isEmpty, drop, append, range, map, take, length)
import Result exposing (withDefault)
import Json.Decode as Json


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
  receiver : Address,
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
  randomValue : Int,
  txForm: TxForm
}

type alias TxForm = {
  sender : String,
  receiver : String,
  amount : Int
}

txHash : Transaction -> String
txHash tx =
  toString tx.amount ++ tx.sender.hash ++ tx.receiver.hash
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
    ", to: " ++ hashDisplay tx.receiver.hash ++ ", amount: " ++
    toString tx.amount ++ "BTC}"

hashDisplay : String -> String
hashDisplay hash = slice 0 10 hash ++ "..."

newMiner : Maybe Block -> Miner
newMiner block = { blockToErase = block }

newAddress : Int -> Int -> Address
newAddress seed balance = { hash = sha256 (toString seed), balance = balance }

newTx : Int -> Int -> Int -> Transaction
newTx senderSeed receiverSeed amount =
  {
    sender = newAddress senderSeed 10,
    receiver = newAddress receiverSeed 10,
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

findAddress : String -> List Address -> Address
findAddress hash addresses =
  let
    matches = addresses
      |> filter (\a -> a.hash == hash)
  in
    case head matches of
      Nothing ->
        { hash = "", balance = 0 }
      Just address ->
        address

updateBalance : List Address -> Transaction -> List Address
updateBalance addressBook transaction =
  addressBook
    |> map (\address ->
      if address.hash == transaction.sender.hash
        then { address | balance = address.balance - transaction.amount }
      else if address.hash == transaction.receiver.hash
        then { address | balance = address.balance + transaction.amount }
      else
        address
    )


init : ( Model, Cmd Msg )
init =
  (
    {
      miners = range 0 19
        |> map (\n -> newMiner Nothing),
      discoveredBlocks = [OriginBlock],
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



---- UPDATE ----


type Msg = Next | PostTx | InputTxSender String | InputTxReceiver String | InputTxAmount String | RandomEvent Int

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
            newTxReceiver = length model.addressBook + 1
            newTxAmount = 1
            newAddressBalance = 10
            removeMinedTx = (drop 1 model.transactionPool)
            replacementTx = newTx newTxSender newTxReceiver newTxAmount
            newTransactionPool = if (length model.transactionPool) <= 4
              then append removeMinedTx [replacementTx]
              else removeMinedTx
            withNewAddresses = (append model.addressBook [
              (newAddress newTxSender newAddressBalance),
              (newAddress newTxReceiver newAddressBalance)
            ])
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
                  transactionPool = newTransactionPool,
                  addressBook = updateBalance withNewAddresses transaction
                }

inputTxSender : Model -> String -> Model
inputTxSender model value =
  let
    oldForm = model.txForm
    newForm = { oldForm | sender = value }
  in
    { model | txForm = newForm }

inputTxReceiver : Model -> String -> Model
inputTxReceiver model value =
  let
    oldForm = model.txForm
    newForm = { oldForm | receiver = value }
  in
    { model | txForm = newForm }

inputTxAmount : Model -> String -> Model
inputTxAmount model value =
  let
    oldForm = model.txForm
    newForm = { oldForm | amount = withDefault 0 (toInt value) }
  in
    { model | txForm = newForm }

postTx : Model -> Model
postTx model =
  let
    newTransaction = {
      sender = findAddress model.txForm.sender model.addressBook,
      receiver = findAddress model.txForm.receiver model.addressBook,
      amount = model.txForm.amount
    }
  in
    { model | transactionPool = append model.transactionPool [newTransaction] }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Next ->
      ( mine model, Random.generate RandomEvent (Random.int 1 100000) )
    PostTx ->
      ( postTx model, Cmd.none )
    InputTxSender value ->
      ( inputTxSender model value, Cmd.none )
    InputTxReceiver value ->
      ( inputTxReceiver model value, Cmd.none )
    InputTxAmount value ->
      ( inputTxAmount model value, Cmd.none )
    RandomEvent randomValue ->
      ({ model | randomValue = randomValue }, Cmd.none)


---- VIEW ----

-- https://github.com/tbasse/elm-spinner/commit/f96315660354612db67bea37983dab840d389859
onChange : (String -> msg) -> Html.Attribute msg
onChange handler =
    on "change" <| Json.map handler <| Json.at ["target", "value"] Json.string

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
      |> div [],
    h2 [] [ text "Send Transaction" ],
    form [ onSubmit PostTx ] [
      text("From: "),
      select [ onChange InputTxSender ] (
        take numMainAddresses model.addressBook
          |> map ( \address ->
              option [ value address.hash ] [ text (hashDisplay address.hash) ]
            )
      ),
      br [] [],
      text("To: "),
      select [ onChange InputTxReceiver ] (
        take numMainAddresses model.addressBook
          |> map ( \address ->
              option [ value address.hash ] [ text (hashDisplay address.hash) ]
            )
      ),
      br [] [],
      text("Amount: "),
      input [ onInput InputTxAmount, type_ "number", Html.Attributes.min "0" ] [],
      br [] [],
      input [ type_ "submit" ] []
    ]
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
