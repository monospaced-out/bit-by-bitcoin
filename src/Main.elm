module Main exposing (..)

import Html exposing (Html, button, text, div, h1, h2, br)
-- import Html.Attributes exposing (src)
import Html.Events exposing (onClick)



---- MODEL ----


type BlockChain = Blockchain (List Block)

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
  originBlock : Maybe Block,
  transactionPool : List Transaction,
  fillerAddresses : List Address,
  mainAddresses : List Address
}

txHash : Transaction -> String
txHash tx = "asdf"

init : ( Model, Cmd Msg )
init =
  (
    {
      miners = [
        { blockToErase = Nothing },
        { blockToErase = Nothing },
        { blockToErase = Nothing },
        { blockToErase = Nothing },
        { blockToErase = Nothing }
      ],
      originBlock = Nothing,
      transactionPool = [],
      fillerAddresses = [],
      mainAddresses = []
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
          text (toString miner),
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
