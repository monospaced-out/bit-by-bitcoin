module View exposing (..)

import Html exposing (Html, Attribute, button, text, div, h1, h2, h3, br, form, input, select, option, span, i)
import Html.Attributes exposing (type_, value, class)
import Html.Events exposing (on, onClick, onSubmit, onInput)
import Model exposing (Msg(Next, SelectEraseBlock, PostTx, InputTxSender, InputTxReceiver, InputTxAmount), Model, BlockLink(BlockLink, NoBlock), Miner, Transaction, Address, blockLinkHash, blockHash, testBlockHash, txHash, longestChain, withUpdatedBalances, balanceFor, confirmedBalanceFor, nextTx, nonceFor, isValidTx, erasableBlocks, blockToMine, isValidHash, isBlockInChain)
import Settings exposing (confirmationsRequired, numMainAddresses)
import List exposing (indexedMap, map, filter, append, concat, concatMap, take, head, reverse, length, drop)
import String exposing (slice)
import Array exposing (Array, fromList, toList, get)
import Json.Decode as Json

-- https://github.com/tbasse/elm-spinner/commit/f96315660354612db67bea37983dab840d389859
onChange : (String -> msg) -> Html.Attribute msg
onChange handler =
    on "change" <| Json.map handler <| Json.at ["target", "value"] Json.string

view : Model -> Html Msg
view model = div [ class "container" ]
  [
    div [ class "left-pane" ] [
      div [ class "left-pane-content" ] [
        h1 [] [ text "bit by bitcoin" ],
        button [ onClick Next ] [ text "Next" ]
      ]
    ],
    div [ class  "center-pane" ] [
      div [ class "center-pane-content" ] [
        blockChain model model.discoveredBlocks
      ]
    ],
    div [ class "right-pane" ] [
      div [ class "right-pane-content" ] [
        h2 [] [ text "Addresses" ],
        div [ class "flex-container" ] [
          div [ class "address-container" ] [
            htmlAddresses model
          ],
          htmlTransactionForm model
        ],
        h2 [] [ text "Transaction Pool" ],
        htmlTransactionPool model,
        h2 [] [ text "Miners" ],
        htmlMiners model
      ]
    ]
  ]

blockChain : Model -> List BlockLink -> Html msg
blockChain model blockLinks =
  div [ class "tree" ] [
    let
      blocksInOrder = reverse blockLinks
    in
      case head blocksInOrder of
        Nothing ->
          span [] []
        Just nextBlock ->
          div [ class "leaves-container" ] [ buildBlockTree model blocksInOrder nextBlock ]
  ]

buildBlockTree : Model -> List BlockLink -> BlockLink -> Html msg
buildBlockTree model allBlockLinks blockLink =
  div [ class "node" ] (
    case blockLink of
      NoBlock ->
        [
          allBlockLinks
            |> childrenForOriginBlock
            |> htmlBlockChildren model allBlockLinks,
          htmlBlock model NoBlock
        ]
      BlockLink block ->
        [
          block.nextBlocks
            |> map (\blockLinkIndex ->
                getBlockLink blockLinkIndex allBlockLinks
              )
            |> htmlBlockChildren model allBlockLinks,
          htmlBlock model (BlockLink block)
        ]
  )

childrenForOriginBlock : List BlockLink -> List BlockLink
childrenForOriginBlock allBlockLinks =
  allBlockLinks
    |> filter (\bl ->
        case bl of
          NoBlock -> False
          BlockLink block ->
            case block.previousBlock of
              NoBlock -> True
              BlockLink block -> False
      )

getBlockLink : Int -> List BlockLink -> BlockLink
getBlockLink index blockLinks =
  case get index (fromList blockLinks) of
    Nothing ->
      NoBlock
    Just blockLinkAtIndex ->
      blockLinkAtIndex

htmlBlock : Model -> BlockLink -> Html msg
htmlBlock model blockLink =
  div [ class "block-container" ] [
    div [ blockClass model blockLink ] [
      i [ class "fas fa-square" ] [],
      text (hashDisplay (blockLinkHash blockLink))
    ]
  ]

blockClass : Model -> BlockLink -> Attribute msg
blockClass model blockLink =
  let
    blockchain = longestChain model.discoveredBlocks
    isInBlockChain = isBlockInChain blockLink blockchain
    isConfirmed = isBlockInChain blockLink (drop confirmationsRequired blockchain)
  in
    if isInBlockChain
      then
        if isConfirmed
          then class "block block-confirmed"
        else
          class "block block-unconfirmed"
    else class "block block-ignored"

htmlBlockChildren : Model -> List BlockLink -> List BlockLink -> Html msg
htmlBlockChildren model allBlockLinks childrenBlockLinks =
  let
    leavesContainerClass =
      if length childrenBlockLinks > 1
        then "leaves-container multiple-leaves"
        else "leaves-container"
  in
    div [ class leavesContainerClass ] (
      childrenBlockLinks
        |> map (\childBlockLink -> buildBlockTree model allBlockLinks childBlockLink )
    )

htmlAddresses : Model -> Html Msg
htmlAddresses model =
  take numMainAddresses model.addressBook
    |> concatMap ( \address ->
        let
          blockchain = longestChain model.discoveredBlocks
          confirmedBalance = confirmedBalanceFor blockchain address
          unconfirmedBalance = balanceFor blockchain address
        in
          [
            i [ class "fas fa-address-card" ] [],
            hashDisplay address.hash ++ " | " ++ toString confirmedBalance |> text,
            i [ class "fab fa-bitcoin" ] [],
            if unconfirmedBalance /= confirmedBalance
              then span [] [
                " (pending: " ++ toString unconfirmedBalance |> text,
                i [ class "fab fa-bitcoin" ] [],
                ")" |> text
              ]
            else text "",
            br [] []
          ]
      )
    |> div [ class "address" ]

htmlTransactionForm : Model -> Html Msg
htmlTransactionForm model =
  form [ class "transaction-form", onSubmit PostTx ] [
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

htmlTransactionPool : Model -> Html Msg
htmlTransactionPool model =
  model.transactionPool
    |> reverse
    |> map ( \tx ->
        div [ txClass (longestChain model.discoveredBlocks) tx ] [
          i [ class "fas fa-exchange-alt" ] [],
          hashDisplay (txHash tx) ++ " | " |> text,
          i [ class "fas fa-address-card" ] [],
          hashDisplay tx.sender.hash |> text,
          i [ class "fas fa-arrow-right" ] [],
          i [ class "fas fa-address-card" ] [],
          hashDisplay tx.receiver.hash ++ " | " ++ toString tx.amount |> text,
          i [ class "fab fa-bitcoin" ] []
        ]
      )
    |> div []

htmlMiners : Model -> Html Msg
htmlMiners model =
  model.miners
    |> indexedMap ( \m miner ->
        div [ minerClass model m miner ] [
          htmlMiner model m miner
        ]
      )
    |> div [ class "miners" ]

htmlMiner : Model -> Int -> Miner -> Html Msg
htmlMiner model minerIndex miner =
  let
    maybeNextTransaction = nextTx (longestChain model.discoveredBlocks) model.transactionPool
    previousBlock = blockToMine model miner
  in
    case maybeNextTransaction of
      Nothing -> "" |> text
      Just nextTransaction ->
        div [ class "miner" ] [
          div [ class "miner-body" ] [
            div [ class "miner-input" ] [
              div [ class "miner-input-row" ] [
                div [ class "miner-input-label" ] [
                  "nonce:" |> text
                ],
                div [ class "miner-input-value nonce" ] [
                  nonceFor minerIndex model.randomValue |> text
                ]
              ],
              div [ class "miner-input-row" ] [
                div [ class "miner-input-label" ] [
                  "transaction:" |> text
                ],
                div [ class "miner-input-value" ] [
                  i [ class "fas fa-exchange-alt" ] [],
                  txHash nextTransaction |> hashDisplay |> text
                ]
              ],
              div [ class "miner-input-row" ] [
                div [ class "miner-input-label" ] [
                  "previous block:" |> text
                ],
                div [ class "miner-input-value" ] [
                  i [ class "fas fa-square" ] [],
                  blockLinkHash previousBlock |> hashDisplay |> text
                ]
              ]
            ],
            div [ class "miner-output" ] [
              testBlockHash nextTransaction previousBlock minerIndex model.randomValue
                |> hashDisplay
                |> text,
              i [ class "fas fa-calculator" ] []
            ]
          ],
          select [ onChange (\s -> SelectEraseBlock s minerIndex), value (blockLinkHash miner.blockToErase) ] (
            model.discoveredBlocks
              |> erasableBlocks
              |> map ( \blocklink ->
                  option [ value (blockLinkHash blocklink) ] [ text (hashDisplay (blockLinkHash blocklink)) ]
                )
              |> append [ option [ value (blockLinkHash NoBlock) ] [ text "Block to erase" ] ]
          )
        ]

minerDisplay : Miner -> String
minerDisplay miner =
  case miner.blockToErase of
    NoBlock ->
      "Honest"
    BlockLink block ->
      "Erasing block: " ++ hashDisplay (blockHash block)

minerActionDisplay : Model -> Int -> String
minerActionDisplay model minerIndex =
  let
    nextTransaction = nextTx (longestChain model.discoveredBlocks) model.transactionPool
    previousBlock = head (longestChain model.discoveredBlocks)
  in
    case nextTransaction of
      Nothing ->
        "no next"
      Just transaction ->
        case previousBlock of
          Nothing ->
            "no previous"
          Just block ->
            " | trying nonce " ++ nonceFor minerIndex model.randomValue ++ ": " ++
              hashDisplay (testBlockHash transaction block minerIndex model.randomValue)

blockDisplay : BlockLink -> String
blockDisplay blocklink =
  hashDisplay (blockLinkHash blocklink) ++
  case blocklink of
    NoBlock ->
      " (origin block)"
    BlockLink block ->
      " {transaction: " ++ txDisplay block.transaction ++
        ", nonce: " ++ block.nonce ++ ", previousBlock: " ++
        hashDisplay (blockLinkHash block.previousBlock) ++ "}"

txDisplay : Transaction -> String
txDisplay tx =
  let
    amount = toString tx.amount ++ "BTC"
    motion = hashDisplay tx.sender.hash ++ " -> " ++ hashDisplay tx.receiver.hash
  in
    motion ++ " | " ++ amount

hashDisplay : String -> String
hashDisplay hash = slice 0 7 hash

minerClass : Model -> Int -> Miner -> Attribute msg
minerClass model minerIndex miner =
  let
    previousBlock = blockToMine model miner
    nextTransaction = nextTx (longestChain model.discoveredBlocks) model.transactionPool
  in
    case nextTransaction of
      Nothing ->
        class ""
      Just transaction ->
        if isValidHash (testBlockHash transaction previousBlock minerIndex model.randomValue)
          then class "miner-success"
          else class ""

txClass : List BlockLink -> Transaction -> Attribute msg
txClass blockchain transaction =
  if isValidTx blockchain transaction
    then class "transaction"
  else
    class "transaction invalid-transaction"
