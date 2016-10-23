{-# LANGUAGE RecordWildCards #-}

module Tronkell.Game.Engine where

import Control.Monad.State.Strict
import Data.Maybe (fromJust, catMaybes)
import qualified Data.Map as Map

import Tronkell.Types
import Tronkell.Game.Types

gameEngine :: GameEngine
gameEngine = fmap concat . mapM runEvent

runEngine :: GameEngine -> Game -> [InputEvent] -> ([OutEvent], Game)
runEngine engine game inEvents = runState (engine inEvents) game

runEvent :: InputEvent -> State Game [OutEvent]
runEvent inputEvent = do
  game <- get

  if not $ isValidEvent inputEvent game && isGameInProgress game
  then return []
  else do
    outEvents <- case inputEvent of
      TurnLeft  pid -> turnLeft pid
      TurnRight pid -> turnRight pid
      Tick           -> tick
      PlayerQuit pid -> playerQuit pid

    statusEvents <- setGameStatus
    return $ outEvents ++ statusEvents

setGameStatus :: State Game [OutEvent]
setGameStatus = do
  game <- get
  let alivePlayers   = filter ((== Alive) . playerStatus) $ Map.elems (gamePlayers game)
      noAlivePlayers = length alivePlayers
      winner         = if noAlivePlayers == 1 then Just (head alivePlayers) else Nothing
      status         = if noAlivePlayers > 1 then InProgress else Finished
      game'          = game { gameStatus  = status,
                              gameWinner  = winner
                            }
  put game'
  return $ case status of
    InProgress -> []
    Finished -> [GameEnded (fmap playerId winner)]

isValidEvent :: InputEvent -> Game -> Bool
isValidEvent event game = case event of
  Tick           -> True
  TurnLeft  pid -> isPlayerAlive pid game
  TurnRight pid -> isPlayerAlive pid game
  PlayerQuit pid -> isPlayerAlive pid game

isGameInProgress :: Game -> Bool
isGameInProgress = (== InProgress) . gameStatus

isPlayerAlive :: PlayerId -> Game -> Bool
isPlayerAlive pid =
  maybe False ((== Alive) . playerStatus)
  . Map.lookup pid
  . gamePlayers

stopAtBoundary :: GameConfig -> Coordinate -> Coordinate
stopAtBoundary (GameConfig w h _ _) (x, y) =
  (min (w - 1) (max x 0),
   min (h - 1) (max y 0))

computePlayerStatus :: GameConfig -> Coordinate -> PlayerStatus
computePlayerStatus (GameConfig w h _ _) (x, y) =
  if x < 0 || x >= w || y < 0 || y >= h
  then Dead
  else Alive

movePlayerForward :: GameConfig -> Player -> (Player, [OutEvent])
movePlayerForward gameConfig player@Player{..} =
  if playerStatus == Dead
  then (player, []) -- Are we eating some bad state bug ?
  else let (x,y)       = playerCoordinate
           newPosition = case playerOrientation of
                      North -> (x, y-1)
                      East  -> (x+1, y)
                      South -> (x, y+1)
                      West  -> (x-1, y)
           newCoordinate = stopAtBoundary gameConfig newPosition
           newStatus     = computePlayerStatus gameConfig newPosition
           newPlayer     = player { playerCoordinate = newCoordinate
                                  , playerStatus     = newStatus
                                  , playerTrail      = newCoordinate : playerTrail }
           deadEvents    = case newStatus of
                             Alive -> []
                             Dead  -> [PlayerDied playerId newCoordinate]
       in ( newPlayer
          , PlayerMoved playerId newCoordinate playerOrientation : deadEvents )

tick :: State Game [OutEvent]
tick  = do
  game@Game{..} <- get
  let playersAndMovesMap = Map.map (movePlayerForward gameConfig) gamePlayers
      (playersAfterMove, deadEvents) = playersAfterCollision . Map.map fst $ playersAndMovesMap
      newGame            = game { gamePlayers = playersAfterMove }
      moves              = concat <$> Map.elems . Map.map snd $ playersAndMovesMap

  put newGame
  return $ moves ++ deadEvents

turnRight :: PlayerId -> State Game [OutEvent]
turnRight = turn (getNextEnum 1)

turnLeft :: PlayerId -> State Game [OutEvent]
turnLeft = turn (getNextEnum 3)

playerQuit :: PlayerId -> State Game [OutEvent]
playerQuit nick = do
  game@Game{..} <- get
  let player         = fromJust . Map.lookup nick $ gamePlayers
      newPlayer      = player { playerStatus = Dead }
      newGamePlayers = Map.insert nick newPlayer gamePlayers
  put game { gamePlayers = newGamePlayers }
  return [PlayerDied nick (playerCoordinate newPlayer)]

getNextEnum :: Int -> Orientation -> Orientation
getNextEnum turnTimes i = head . drop (turnTimes + fromEnum i) . cycle $ orientations

turn :: (Orientation -> Orientation) -> PlayerId -> State Game [OutEvent]
turn getNewOrientation nick = do
  game@Game{..} <- get
  let player         = fromJust . Map.lookup nick $ gamePlayers
      newOrientation = getNewOrientation . playerOrientation $ player
      newPlayer      = player { playerOrientation = newOrientation }
      newGamePlayers = Map.insert nick newPlayer gamePlayers
      newGame        = game { gamePlayers = newGamePlayers }
  put newGame
  return [PlayerMoved nick (playerCoordinate newPlayer) newOrientation]

orientations :: [Orientation]
orientations = [minBound..maxBound]

deadPlayers :: Game -> [PlayerId]
deadPlayers = Map.keys . Map.filter ((== Dead) . playerStatus) . gamePlayers

playersAfterCollision :: Map.Map PlayerId Player -> (Map.Map PlayerId Player, [OutEvent])
playersAfterCollision players =
  let playersWithDeadEvent =
        Map.map (\ player ->
                     if isHeadInATrail player players
                     then ( player { playerStatus = Dead }
                          , Just $ PlayerDied (playerId player) (playerCoordinate player) )
                     else (player, Nothing))
                players
  in ( Map.map fst playersWithDeadEvent
     , catMaybes . Map.elems . Map.map snd $ playersWithDeadEvent )
  where
    isHeadInATrail player allPlayers =
      any (\ player2 -> (playerId player) /= (playerId player2) &&
                        (playerCoordinate player) `elem` (playerTrail player2))
          allPlayers
