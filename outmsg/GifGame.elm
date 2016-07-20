module GifGame exposing (update, view, init, Model, OutMsg(..), Msg)

-- (Model, InternalMsg, init, Translator, translator, view, update)

import Html exposing (Html, div, text, p, button, input, br, img)
import Html.App exposing (program)
import Html.Attributes exposing (style, src)
import Html.Events exposing (onClick, onInput)
import Http
import Random
import Task
import Json.Decode as Json
import Random.Extra exposing (constant, choices)


type alias Model =
    { gifUrl : String
    , currentTopic : String
    , currentGuess : String
    , guessesLeft : Int
    }


type Msg
    = TextChanged String
    | MakeGuess
    | GifError Http.Error
    | NewTopic String
    | NewGif String


type OutMsg
    = PlayerWin Int
    | PlayerLoss



{-
      type alias TranslationDictionary parentMsg =
          { onInternalMessage : InternalMsg -> parentMsg
          , onPlayerWin : Int -> parentMsg
          , onPlayerLose : parentMsg
          }


   type alias Translator parentMsg =
       Msg -> parentMsg


   translator : TranslationDictionary parentMsg -> Translator parentMsg
   translator { onInternalMessage, onPlayerWin, onPlayerLose } msg =
       case msg of
           ForSelf internal ->
               onInternalMessage internal

           ForParent (PlayerWin score) ->
               onPlayerWin score

           ForParent PlayerLoss ->
               onPlayerLose


-}
-- INIT


initialModel =
    { gifUrl = "waiting.gif", currentTopic = "", currentGuess = "", guessesLeft = 10 }


init : ( Model, Cmd Msg )
init =
    initialModel ! [ getRandomTopic ]



-- VIEW


{-| Note the use of composition here to chain the `TextChanged` constructor function with `ForSelf`
-}
view : Model -> Html Msg
view model =
    div [ style [ ( "display", "block" ), ( "padding", "20px" ) ] ]
        [ img [ src model.gifUrl ] []
        , br [] []
        , input [ onInput (TextChanged) ] []
        , button [ onClick (MakeGuess) ] [ text "Guess!" ]
        , p [] [ text ("Guesses left: " ++ toString model.guessesLeft) ]
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update msg model =
    case msg of
        TextChanged newText ->
            ( { model | currentGuess = newText }, Cmd.none, Nothing )

        GifError _ ->
            ( model, getRandomGif model.currentTopic, Nothing )

        NewTopic topic ->
            ( { model | currentTopic = topic }, getRandomGif topic, Nothing )

        NewGif gifUrl ->
            ( { model | gifUrl = gifUrl }, Cmd.none, Nothing )

        MakeGuess ->
            let
                newGame =
                    { initialModel | currentGuess = model.currentGuess }
            in
                if model.currentGuess == model.currentTopic then
                    ( newGame, getRandomTopic, Just <| PlayerWin model.guessesLeft )
                else if model.guessesLeft == 1 then
                    ( newGame, getRandomTopic, Just PlayerLoss )
                else
                    ( { model | guessesLeft = model.guessesLeft - 1 }, Cmd.none, Nothing )


never : Never -> a
never n =
    never n



{-
   generateParentMsg : OutMsg -> Cmd Msg
   generateParentMsg outMsg =
       Task.perform never ForParent (Task.succeed outMsg)

-}


getRandomTopic : Cmd Msg
getRandomTopic =
    let
        -- Just some things I like...
        topics =
            [ "cats", "dogs", "orphan black", "elm", "translation", "pets" ]

        topicGenerators =
            List.map constant topics

        randomTopicGenerator =
            choices topicGenerators
    in
        Random.generate (NewTopic) randomTopicGenerator


getRandomGif : String -> Cmd Msg
getRandomGif topic =
    let
        decodeGifUrl : Json.Decoder String
        decodeGifUrl =
            Json.at [ "data", "image_url" ] Json.string

        url =
            "http://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
    in
        Task.perform GifError NewGif (Http.get decodeGifUrl url)
