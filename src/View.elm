module View exposing (..)

import Html exposing (Html, Attribute, button, text, div, h1, h2, h3, br, form, input, select, option, span, i)
import Html.Attributes exposing (style, type_, value, class)
import Html.Events exposing (on, onClick, onSubmit, onInput)
import Model exposing (Msg(Next, SelectEraseBlock, PostTx, InputTxSender, InputTxReceiver, InputTxAmount), Model, BlockLink(BlockLink, NoBlock), Miner, Transaction, Address, blockLinkHash, blockHash, testBlockHash, txHash, longestChain, withUpdatedBalances, balanceFor, confirmedBalanceFor, nextTx, nonceFor, isValidTx, erasableBlocks, blockToMine, isValidHash)
import Settings exposing (confirmationsRequired, numMainAddresses)
import List exposing (indexedMap, map, filter, append, concat, concatMap, take, head, reverse, length)
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
        blockChain model.discoveredBlocks
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

blockChain : List BlockLink -> Html msg
blockChain blockLinks =
  div [ class "tree" ] [
    let
      blocksInOrder = reverse blockLinks
    in
      case head blocksInOrder of
        Nothing ->
          span [] []
        Just nextBlock ->
          div [ class "leaves-container" ] [ buildBlockTree blocksInOrder nextBlock ]
  ]

buildBlockTree : List BlockLink -> BlockLink -> Html msg
buildBlockTree allBlockLinks blockLink =
  div [ class "node" ] (
    case blockLink of
      NoBlock ->
        [
          htmlBlock NoBlock,
          allBlockLinks
            |> childrenForOriginBlock
            |> htmlBlockChildren allBlockLinks
        ]
      BlockLink block ->
        [
          htmlBlock (BlockLink block),
          block.nextBlocks
            |> map (\blockLinkIndex ->
                getBlockLink blockLinkIndex allBlockLinks
              )
            |> htmlBlockChildren allBlockLinks
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

htmlBlock : BlockLink -> Html msg
htmlBlock blockLink =
  div [ class "block-container" ] [
    div [ class "block" ] [ text (hashDisplay (blockLinkHash blockLink)) ]
  ]

htmlBlockChildren : List BlockLink -> List BlockLink -> Html msg
htmlBlockChildren allBlockLinks childrenBlockLinks =
  let
    leavesContainerClass =
      if length childrenBlockLinks > 1
        then "leaves-container multiple-leaves"
        else "leaves-container"
  in
    div [ class leavesContainerClass ] (
      childrenBlockLinks
        |> map (\childBlockLink -> buildBlockTree allBlockLinks childBlockLink )
    )

htmlAddresses : Model -> Html Msg
htmlAddresses model =
  take numMainAddresses model.addressBook
    |> concatMap ( \address ->
      let
        blockchain = longestChain model.discoveredBlocks
        confirmedBalance = confirmedBalanceFor blockchain address
        unconfirmedBalance = balanceFor blockchain address
        confirmedBalanceString = hashDisplay address.hash ++ " " ++
          toString confirmedBalance ++ " BTC"
        unconfirmedBalanceString = " (unconfirmed: " ++ toString unconfirmedBalance ++
          " BTC)"
        displayString =
          if unconfirmedBalance == confirmedBalance
            then confirmedBalanceString
          else
            confirmedBalanceString ++ unconfirmedBalanceString
      in
        [
          text displayString,
          br [] []
        ] )
    |> div []

htmlTransactionForm : Model -> Html Msg
htmlTransactionForm model =
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

htmlTransactionPool : Model -> Html Msg
htmlTransactionPool model =
  model.transactionPool
    |> reverse
    |> map ( \tx ->
        div [ class "transaction", txStyle (longestChain model.discoveredBlocks) tx ] [
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
        div [ minerStyle model m miner ] [
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
                  txHash nextTransaction |> hashDisplay |> text
                ]
              ],
              div [ class "miner-input-row" ] [
                div [ class "miner-input-label" ] [
                  "prev block:" |> text
                ],
                div [ class "miner-input-value" ] [
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
              |> reverse
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

minerStyle : Model -> Int -> Miner -> Attribute msg
minerStyle model minerIndex miner =
  let
    previousBlock = blockToMine model miner
    nextTransaction = nextTx (longestChain model.discoveredBlocks) model.transactionPool
  in
    case nextTransaction of
      Nothing ->
        style []
      Just transaction ->
        if isValidHash (testBlockHash transaction previousBlock minerIndex model.randomValue)
          then style [ ("backgroundColor", "green") , ("color", "white") ]
          else style []

txStyle : List BlockLink -> Transaction -> Attribute msg
txStyle blockchain transaction =
  if isValidTx blockchain transaction
    then style []
  else
    style [ ("backgroundColor", "red") , ("color", "white") ]
