module Main where

import qualified System.Environment as Env
import qualified Data.Maybe as DMay

import Tronkell.Game.Types
import Tronkell.Server.Types
import Tronkell.Server.Server

main :: IO ()
main = do
  envPort <- Env.lookupEnv "PORT"
  let port = DMay.fromMaybe 8034 (read <$> envPort)
  startServer (ServerConfig port) $ GameConfig 80 60 1 10
  -- let p1 = Player (PlayerNick "player 1") Alive (1,1) North []
  --     p2 = Player (PlayerNick "player 2") Alive (2,2) North []
  --     config = GameConfig 3 3 1 1
  --     gamePs = Map.fromList [(playerNick p1, p1), (playerNick p2, p2)]
  --     game = Game Nothing gamePs InProgress config
  --     engine = gameEngine [TurnLeft (playerNick p1), Tick] in
  --   print $ runState engine game
