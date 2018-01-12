module Main exposing (..)

import Html exposing (Html, button, text, div, h1, img)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)



---- MODEL ----


type BlockChain = Blockchain (List Block)

type alias Miner =
  { blockToErase : Maybe Block }

type alias Block =
  { transaction : Transaction
  , chain : BlockChain
  , nonce : String
  }

type alias Transaction =
  { sender : Address
  , recipient : Address
  , amount : Int
  }

type alias Address =
  { hash : String }

type alias Model =
    { miners : List Miner
    , originBlock : Maybe Block
    , transactionPool : List Transaction
    , fillerAddresses : List Address
    , mainAddresses : List Address
    }

init : ( Model, Cmd Msg )
init =
    (
      { miners = []
      , originBlock = Nothing
      , transactionPool = []
      , fillerAddresses = []
      , mainAddresses = []
      }
      , Cmd.none
    )



---- UPDATE ----


type Msg = Next


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (
    case msg of
      Next ->
        model
    , Cmd.none
    )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , div [] [ text ("# Miners: " ++ toString (List.length (.miners model))) ]
        , button [ onClick Next ] [ text "Next" ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
