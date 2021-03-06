module Update exposing (..)

import Model exposing (Msg(Next, PostTx, InputTxSender, InputTxReceiver, InputTxAmount, SelectEraseBlock, RandomEvent, GetNames, ProvideNames), Model, BlockLink(BlockLink, NoBlock), nextTx, longestChain, maliciousBlockToMine, nonceFor, testBlockHash, txHash, blockHash, blockLinkHash, isValidTx, newTx, newAddress, findAddress, isBlockInChain, blockToMine, isValidHash, minedBlocksFor)
import Random
import Array exposing(fromList, get)
import List exposing (head, indexedMap, filter, drop, length, append, map)
import String exposing (slice, toInt)
import Settings exposing (transactionPoolSize)
import Result exposing (withDefault)
import Ports exposing (getNames)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Next ->
      (
        model |> mine |> refillTransactionPool |> updateMiners,
        Cmd.batch [
          Random.generate RandomEvent (Random.int 1 100000),
          getNames (transactionPoolSize * 2)
        ]
      )
    PostTx ->
      ( postTx model, Cmd.none )
    InputTxSender value ->
      ( inputTxSender model value, Cmd.none )
    InputTxReceiver value ->
      ( inputTxReceiver model value, Cmd.none )
    InputTxAmount value ->
      ( inputTxAmount model value, Cmd.none )
    SelectEraseBlock value index ->
      ( selectEraseBlock model value index, Cmd.none )
    RandomEvent randomValue ->
      ({ model | randomValue = randomValue }, Cmd.none)
    GetNames numNames ->
      (model, getNames numNames )
    ProvideNames names ->
      ({ model | randomNames = names }, Cmd.none)

mine : Model -> Model
mine model =
  let
    nextTransaction = nextTx (longestChain model.discoveredBlocks) model.transactionPool
    previousBlock = head (longestChain model.discoveredBlocks)
  in
    case nextTransaction of
      Nothing ->
        model
      Just transaction ->
        case previousBlock of
          Nothing ->
            model
          Just longestChainBlock ->
            let
              results = minedBlocksFor model transaction
              withoutMinedTx = model.transactionPool
                |> filter (\tx -> txHash tx /= txHash transaction )
              withoutNextTxInvalid =
                case head withoutMinedTx of
                  Nothing ->
                    withoutMinedTx
                  Just tx ->
                    if isValidTx (longestChain model.discoveredBlocks) tx
                      then withoutMinedTx
                    else
                      drop 1 withoutMinedTx
            in
              case head results of
                Nothing ->
                  model
                Just (nonce, hash, block) ->
                  let
                    newBlock = BlockLink {
                      transaction = transaction,
                      previousBlock = block,
                      nextBlocks = [],
                      nonce = nonce,
                      hashCache = hash
                    }
                    newBlockIndex = length model.discoveredBlocks
                    withUpdatedPreviousBlock = setNextBlock block newBlockIndex model.discoveredBlocks
                  in
                    { model |
                      discoveredBlocks = newBlock :: withUpdatedPreviousBlock,
                      transactionPool = withoutNextTxInvalid
                    }

refillTransactionPool : Model -> Model
refillTransactionPool model =
  let
    namesArray = fromList model.randomNames
    firstNameInList = case get 0 namesArray of
      Nothing -> "Sender"
      Just name -> name
    secondNameInList = case get 1 namesArray of
      Nothing -> "Receiver"
      Just name -> name
    newAddressBalance = 10
    newTxSenderSeed = length model.addressBook
    newTxReceiverSeed = length model.addressBook + 1
    newTxAmount = 1
    txToAdd = newTx (firstNameInList, newTxSenderSeed) (secondNameInList, newTxReceiverSeed) newTxAmount
    withNewTx = append model.transactionPool [ txToAdd ]
    withNewAddresses = (append model.addressBook [
      (newAddress firstNameInList newTxSenderSeed newAddressBalance),
      (newAddress secondNameInList newTxReceiverSeed newAddressBalance)
    ])
  in
    if length model.transactionPool < transactionPoolSize
      then refillTransactionPool { model |
        transactionPool = withNewTx,
        addressBook = withNewAddresses,
        randomNames = drop 2 model.randomNames
      }
    else
      model

updateMiners : Model -> Model
updateMiners model =
  { model | miners =
    model.miners
      |> map (\miner ->
          let blockToErase =
            if isBlockInChain miner.blockToErase (longestChain model.discoveredBlocks)
              then miner.blockToErase
            else
              NoBlock
          in
            { miner | blockToErase = blockToErase }
        )
  }

setNextBlock : BlockLink -> Int -> List BlockLink -> List BlockLink
setNextBlock oldBlock newBlockIndex allBlocks =
  allBlocks
    |> map (\blocklink ->
        case blocklink of
          NoBlock ->
            blocklink
          BlockLink block ->
            if blockHash block == blockLinkHash oldBlock
              then BlockLink { block | nextBlocks = newBlockIndex :: block.nextBlocks }
            else
              BlockLink block
      )

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

selectEraseBlock : Model -> String -> Int -> Model
selectEraseBlock model hash index =
  let
    matchingBlocks = model.discoveredBlocks
      |> filter (\blocklink -> (blockLinkHash blocklink) == hash )
    blockToErase =
      case head matchingBlocks of
        Nothing ->
          NoBlock
        Just blocklink ->
          blocklink
    updatedMiners = model.miners
      |> indexedMap (\m miner ->
          if m == index
            then { miner | blockToErase = blockToErase }
          else
            miner
        )
  in
    { model | miners = updatedMiners }

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
