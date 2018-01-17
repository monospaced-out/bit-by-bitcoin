module View exposing (..)

import Html exposing (Html, Attribute, button, text, div, h1, h2, h3, br, form, input, select, option, span)
import Html.Attributes exposing (style, type_, value, class)
import Html.Events exposing (on, onClick, onSubmit, onInput)
import Model exposing (Msg(Next, SelectEraseBlock, PostTx, InputTxSender, InputTxReceiver, InputTxAmount), Model, BlockLink(BlockLink, NoBlock), Miner, Transaction, blockLinkHash, blockHash, testBlockHash, txHash, longestChain, withUpdatedBalances, nextTx, nonceFor, isValidTx)
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
    div [ class "left-pane" ] [ oldView model ],
    div [ class  "center-pane" ] [
      blockChain model.discoveredBlocks
    ],
    div [ class "right-pane" ] []
  ]

oldView : Model -> Html Msg
oldView model = div []
  [
    h1 [] [ text "bit by bitcoin" ],
    button [ onClick Next ] [ text "Next" ],
    h2 [] [ text "Miners" ],
    model.miners
      |> indexedMap ( \m miner ->
          div [ minerStyle model m ] [
            "• " ++ minerDisplay miner |> text,
            minerActionDisplay model m |> text,
            select [ onChange (\s -> SelectEraseBlock s m), value (blockLinkHash miner.blockToErase) ] (
              model.discoveredBlocks
                |> longestChain
                |> filter ( \blocklink ->
                    case blocklink of
                      NoBlock ->
                        False
                      BlockLink block ->
                        True
                  )
                |> reverse
                |> map ( \blocklink ->
                    option [ value (blockLinkHash blocklink) ] [ text (hashDisplay (blockLinkHash blocklink)) ]
                  )
                |> append [ option [ value (blockLinkHash NoBlock) ] [ text "Block to erase" ] ]
            ),
            br [] []
          ]
        )
      |> div [],
    h2 [] [ text "Blockchain" ],
    blockChain model.discoveredBlocks,
    h2 [] [ text "Mined Blocks" ],
    model.discoveredBlocks
      |> indexedMap ( \b blocklink -> [
          if b == confirmationsRequired
            then h3 [] [ text "Confirmed" ]
          else if b == 0
            then h3 [] [ text "Unconfirmed" ]
          else
            text "",
          text ("• " ++ blockDisplay blocklink),
          br [] []
        ] )
      |> concat
      |> div [],
    h2 [] [ text "Transaction Pool" ],
    model.transactionPool
      |> map ( \tx ->
          div [ txStyle (longestChain model.discoveredBlocks) tx ] [
            text ("• " ++ txDisplay tx),
            br [] []
          ]
        )
      |> div [],
    h2 [] [ text "Joe Schmo's Neighborhood" ],
    take numMainAddresses model.addressBook
      |> withUpdatedBalances (longestChain model.discoveredBlocks)
      |> concatMap ( \address -> [
          text ("• " ++ hashDisplay address.hash ++ " " ++ toString address.balance ++ " BTC"),
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

minerDisplay : Miner -> String
minerDisplay miner =
  case miner.blockToErase of
    NoBlock ->
      "just chillin"
    BlockLink block ->
      "Trying to erase block: " ++ hashDisplay (blockHash block)

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
            ". trying nonce " ++ nonceFor minerIndex model.randomValue ++ ": " ++
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
  hashDisplay (txHash tx) ++ " {from: " ++ hashDisplay tx.sender.hash ++
    ", to: " ++ hashDisplay tx.receiver.hash ++ ", amount: " ++
    toString tx.amount ++ "BTC}"

hashDisplay : String -> String
hashDisplay hash = slice 0 10 hash ++ "..."

minerStyle : Model -> Int -> Attribute msg
minerStyle model minerIndex =
  let
    nextTransaction = nextTx (longestChain model.discoveredBlocks) model.transactionPool
  in
    case nextTransaction of
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

txStyle : List BlockLink -> Transaction -> Attribute msg
txStyle blockchain transaction =
  if isValidTx blockchain transaction
    then style []
  else
    style [ ("backgroundColor", "red") , ("color", "white") ]
