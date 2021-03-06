# Tronkell ui in elm

## How to Run :
```
brew install elm
cd ~/tronkell/ui
elm reactor -p 8011
# go to http://localhost:8011/src/Main.elm
```

## Interaction with server:
1. UI would be listening on websockets from server.
2. Starts by registering with name.
3. Sends "ready" when ready to join game. Otherwise "quit" for quitting.
4. Receives game start message from server with details about initial game.
   * Initial game : Game { width, height, players }
   * players : { name, position, orientation }
5. Sends events to server in json.
6. Receives delta-update events from server.
7. Delta-updates means only diffs and not complete game state.


## Implementation details:
1. Based on canvas.
2. Grid of cells.
3. Cell can be empty, player, player's tail.
4. Handles keyboard events of Left key, Right key.
5. Always keeps current state of game.
6. Updates state with delta update events from server.

## Messages from server:
```haskell
data OutMessage = GameReady   Game.GameConfig [Game.Player]
                | PlayerMoved UserID Coordinate Orientation
                | PlayerDied  UserID Coordinate
                | GameEnded   (Maybe UserID)
                | ServerMsg   String
```
Json Msg structure :

```json
1. {"type"    : "GameReady",
    "width"   : Int,
    "height"  : Int,
    "players" : [{ "id"   : Int,
                   "name" : String,
                   "coordinate" : {"x" : Int, "y" : Int"},
                   "orientation" : "North"/"South"/"East"/"West"}]}

2. {"type"   : "PlayerMoved",
    "player" : { "id"          : Int,
                 "coordinate"  : {"x" : Int, "y" : Int"},
                 "orientation" : "North"/"South"/"East"/"West"}
   }
3. {"type"   : "PlayerDied",
    "player" : { "id" : Int }
   }
4. {"type"     : "GameEnded",
    "winnerId" : null / Int
   }
5. {"type"    : "ServeMsg",
    "message" : String
   }
```
## Messages to server:
Id would be appended by the socket handle itself at server.
```
type Msg = PlayerName GM.PlayerName
         | PlayerReady
         | PlayerQuit
         | MoveLeft
         | MoveRight
```
Tcp client : All messages to the server are in string form.
Elm (Websocket client) : Messages are in json format with "type" like "PlayerName", "PlayerReady" etc.
```
"<username>", "ready", "quit", "L", "R"
```
