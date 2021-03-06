module ServerMsg exposing (listenServerMsg, sendServerMsg)

import Message exposing (..)
import Grid.Player as GP
import Grid.Model as GM
import Grid.Message as GMsg

import Keyboard exposing (KeyCode)
import WebSocket
import Json.Decode as Decode exposing ((:=), Decoder)
import Json.Encode as Encode
import Color exposing (Color)
import String

wsserver = "ws://localhost:8331"
playerColors = List.append [Color.yellow, Color.blue, Color.brown] (List.repeat 100 Color.black)

listenServerMsg = WebSocket.listen wsserver (decodeMsg playerColors)

sendServerMsg : Msg -> Cmd Msg
sendServerMsg msg = WebSocket.send wsserver (encodeMsg msg)

decodeMsg : List Color -> String -> Msg
decodeMsg colors json =
    let decodeMsg msgType =
            case msgType of
                "GameReady"  ->
                    Decode.object2 (\ (w, h) ps -> GameReady w h (List.map2 changeColorOfPlayerCell ps colors))
                        ("config" := decodeConfig) ("players" := decodePlayers colors)

                "GameEnded"   -> Decode.object1 GameEnded ("winnerId" := nullOr Decode.int)

                "PlayerDied"  -> Decode.object1 (\id -> GridMsg (GMsg.PlayerDied id)) ("id" := Decode.int)

                "PlayerMoved" ->
                    Decode.object3 (\id coordinate orientation ->
                                        GridMsg (GMsg.PlayerMoved id coordinate orientation))
                        ("id" := Decode.int) ("coordinate" := decodePosition) ("orientation" := decodeOrientation)

                "ServerMsg"        -> Decode.object1 ServerMsg ("message" := Decode.string)

                "PlayerRegisterId" -> Decode.object1 PlayerRegisterId ("id" := Decode.int)

                _                  -> Decode.succeed NoOp
        decoder = Decode.andThen ("type" := Decode.string ) decodeMsg
        res     = Decode.decodeString decoder (Debug.log "received json: " json)
    in case Debug.log "servermsg: " res of
           Ok m -> m
           Err _ -> NoOp

nullOr : Decoder a -> Decoder (Maybe a)
nullOr decoder =
    Decode.oneOf [ Decode.null Nothing
                 , Decode.map Just decoder]

decodePlayer : Color -> Decoder GM.PlayerCell
decodePlayer color =
    (Decode.object4
         (\ id name coord o -> GM.PlayerCell (GP.Player id name color o True []) coord)
         ("id"          := Decode.int)
         ("nick"        := Decode.string)
         ("coordinate"  := decodePosition)
         ("orientation" := decodeOrientation))

decodePlayers : List Color -> Decoder (List GM.PlayerCell)
decodePlayers colors =
    decodeList (List.map decodePlayer colors)

decodeList : List (Decoder a) -> Decoder (List a)
decodeList decoders =
    Decode.customDecoder
        (Decode.list Decode.value)
        (\jsonList ->
             List.foldr (Result.map2 (::)) (Ok [])
                 (List.map2 (\decoder json -> Decode.decodeValue decoder json)
                      decoders jsonList))

decodePosition : Decoder GP.Position
decodePosition = Decode.object2 (\ x y -> (x,y)) ("x" := Decode.float) ("y" := Decode.float)

decodeConfig : Decoder GP.Position
decodeConfig = Decode.object2 (\ x y -> (x,y)) ("width" := Decode.float) ("height" := Decode.float)

decodeOrientation : Decoder GP.Orientation
decodeOrientation =
    Decode.andThen
        Decode.string
        (\ s -> Decode.succeed (case s of
                                    "North" -> GP.North
                                    "East"  -> GP.East
                                    "South" -> GP.South
                                    "West"  -> GP.West
                                    _       -> GP.North))

encodeMsg : Msg -> String
encodeMsg msg =
    let msgObject =
            case msg of
                PlayerReady ->
                    Just [ ("type", Encode.string "Ready") ]
                PlayerName p ->
                    Just [ ("name", Encode.string p)
                         , ("type", Encode.string "Name") ]
                PlayerQuit ->
                    Just [ ("type", Encode.string "Exit") ]
                MoveLeft ->
                    Just [ ("type", Encode.string "Left") ]
                MoveRight ->
                    Just [ ("type", Encode.string "Right") ]

                _ -> Nothing
    in case Debug.log "sending" msgObject of
           Just obj -> obj |> Encode.object |> Encode.encode 0
           Nothing -> ""

changeColorOfPlayerCell : GM.PlayerCell -> Color -> GM.PlayerCell
changeColorOfPlayerCell cell c =
    let p = cell.player
        p' = { p | color = c }
    in { cell | player = p' }
