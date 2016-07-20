module Main exposing (..)

import GifGame as G exposing (OutMsg(..))
import Html exposing (Html, div, button, text)
import Html.App exposing (program)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import OutMessage


-- We have two players playing the game, and keep track
-- of each of their total scores.


type alias Model =
    { p1Game : G.Model
    , p2Game : G.Model
    , p1Score : Int
    , p2Score : Int
    }


type Player
    = P1
    | P2


{-| We can _increase_ a player's score by a certain (variable) amount when she wins; we can penalize a player when she loses, and we can reset the game for both players at once.

  The IncreaseScore and Penalize messages will be generated from within the GifGame component, whereas the Reset message will be generated from Main. The beauty of the Translator pattern is that this doesn't matter: we just list all the messages we will have to handle.
-}
type Msg
    = Reset
    | GameMsg Player G.Msg


{-| Here we present a slight variation on the pattern described in the article. Since we have two players, and therefore two active instances of the GifGame component, we will provide two different translators, one that translates GifGame messages into messages about player 1, and one that translates them into messages about player 2.

  The `translator` function is where this happens -- it takes in a player, and creates a custom translator for that player. Note that the child component doesn't know anything about the Player type, or that it has more than one instance.
-}
interpretOutMsg : Player -> G.OutMsg -> Model -> ( Model, Cmd Msg )
interpretOutMsg player outmsg model =
    let
        modifyP f model =
            case player of
                P1 ->
                    { model | p1Score = f model.p1Score }

                P2 ->
                    { model | p2Score = f model.p2Score }
    in
        case outmsg of
            PlayerWin score ->
                ( modifyP (\currentScore -> score + currentScore) model
                , Cmd.none
                )

            PlayerLoss ->
                ( modifyP (\currentScore -> currentScore - 5) model
                , Cmd.none
                )


{-| As in any Elm app, we need to get initial state and commands from our component. Note that instead of simply Cmd.mapping a "tag" onto the commands we get back, though, we map the translator.
-}
init : ( Model, Cmd Msg )
init =
    let
        ( p1Init, p1Commands ) =
            G.init

        ( p2Init, p2Commands ) =
            G.init
    in
        { p1Game = p1Init
        , p2Game = p2Init
        , p1Score = 0
        , p2Score = 0
        }
            ! [ Cmd.map (GameMsg P1) p1Commands
              , Cmd.map (GameMsg P2) p2Commands
              ]


{-| In the view, we calculate the background color based on the scores of the two players, then create a simple div that renders the two child components. Again, we're mapping p1Translator and p2Translator over the child's Html, rather than a simple tag.
-}
view : Model -> Html Msg
view model =
    let
        backgroundColor =
            if model.p1Score > model.p2Score then
                "lightblue"
            else if model.p2Score > model.p1Score then
                "pink"
            else
                "white"

        bgColorStyle =
            style [ ( "background-color", backgroundColor ) ]
    in
        div [ bgColorStyle ]
            [ Html.App.map (GameMsg P1) (G.view model.p1Game)
            , Html.App.map (GameMsg P2) (G.view model.p2Game)
            , button [ onClick Reset ] [ text "Reset everything" ]
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            init

        GameMsg P1 internal ->
            G.update internal model.p1Game
                |> OutMessage.mapCmd (GameMsg P1)
                |> OutMessage.mapComponent (\newP1 -> { model | p1Game = newP1 })
                |> OutMessage.evaluateMaybe (interpretOutMsg P1) Cmd.none

        GameMsg P2 internal ->
            G.update internal model.p2Game
                |> OutMessage.mapCmd (GameMsg P2)
                |> OutMessage.mapComponent (\newP2 -> { model | p2Game = newP2 })
                |> OutMessage.evaluateMaybe (interpretOutMsg P2) Cmd.none


main =
    program { init = init, view = view, update = update, subscriptions = \_ -> Sub.none }
