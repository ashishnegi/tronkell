{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Tronkell.Game.Types
import Tronkell.Game.Engine

import qualified Data.Map as Map
import Data.List (nub, nubBy)
import Data.Maybe (isNothing)
import System.Random

import Test.Hspec
import Test.QuickCheck

newtype BoundedInt = BoundedInt { unBoundedInt :: Int }
                     deriving (Show, Eq, Ord, Num)

instance Random BoundedInt where
  randomR (BoundedInt lo, BoundedInt hi) g =
    let (a, g') = randomR (lo, hi) g in (BoundedInt a, g')

  random g = let (a, g') = random g in (BoundedInt a, g')

instance Arbitrary BoundedInt where
  arbitrary = fmap BoundedInt $ choose (0, 100)

instance Arbitrary GameConfig where
  arbitrary = do
    w  <- unBoundedInt <$> arbitrary `suchThat` (> 0)
    h  <- unBoundedInt <$> arbitrary `suchThat` (> 0)
    sp <- unBoundedInt <$> arbitrary
    return $ GameConfig w h sp 1

instance Arbitrary Player where
  arbitrary = do
    n <- PlayerNick <$> arbitrary
    x <- unBoundedInt <$> arbitrary
    y <- unBoundedInt <$> arbitrary
    o <- arbitraryBoundedEnum
    return $ Player n Alive (x, y) o []

isValidPlayer :: GameConfig -> Player -> Bool
isValidPlayer (GameConfig w h _ _) (Player _ _ (x, y) _ _) = x < w && y < h

areOverlappingPlayers :: Player -> Player -> Bool
areOverlappingPlayers p1 p2 =
  playerNick p1 == playerNick p2 || playerCoordinate p1 == playerCoordinate p2

instance Arbitrary Game where
  arbitrary = do
    conf   <- arbitrary
    np     <- unBoundedInt
              <$> arbitrary `suchThat` (< (BoundedInt $ gameWidth conf * gameHeight conf))
                            `suchThat` (> 0)
    ps     <- nubBy areOverlappingPlayers
              <$> (vectorOf np $ arbitrary `suchThat` isValidPlayer conf)
    let mp = Map.fromList . map (\p -> (playerNick p, p)) $ ps
    return $ Game Nothing mp InProgress conf

isPlayerOnGrid :: Game -> Player -> Bool
isPlayerOnGrid game = isValidPlayer (gameConfig game)

main :: IO ()
main = hspec $ do
  describe "game init setup" $ do
    it "has all players on grid" $
      property $ \game -> testPlayerProperty (isPlayerOnGrid game) game

    it "has no trails at the start" $
      property $ testPlayerProperty (null . playerTrail)

    it "has all alive players" $
      property $ testPlayerProperty ((== Alive) . playerStatus)

    it "has no overlapping players" $
      property $ \game@Game {..} ->
        length gamePlayers == (length . nub . Map.elems . Map.map playerCoordinate $ gamePlayers)

    it "has No winner at start" $
      property $ isNothing . gameWinner

    it "is in progress at start" $
      property $ (== InProgress) . gameStatus

  describe "game in progress" $ do
    it "has all players on grid after some events" $
      property $ \game nums ->
        let events = map (genEvent game) nums
            (_, game') = runEngine gameEngine game events
        in testPlayerProperty (isPlayerOnGrid game') game'

  where
    testPlayerProperty f = and . Map.map f . gamePlayers

    genEvent :: Game -> (Positive Int, Positive Int) -> InputEvent
    genEvent Game {..} (Positive eventNo, Positive playerNo) =
      let nick = (Map.keys gamePlayers) !! (playerNo `mod` Map.size gamePlayers)
      in case eventNo `mod` 3 of
        0 -> Tick
        1 -> TurnLeft nick
        2 -> TurnRight nick
