{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module TestUtils where

import System.Random
import Data.String (fromString)
import Data.List (nub, nubBy)
import qualified Data.Map as Map

import Tronkell.Types
import Tronkell.Game.Types
import Tronkell.Game.Engine

import Test.Hspec
import Test.QuickCheck

newtype BoundedInt = BoundedInt { unBoundedInt :: Int }
                     deriving (Show, Eq, Ord, Num)

instance Random BoundedInt where
  randomR (BoundedInt lo, BoundedInt hi) g =
    let (a, g') = randomR (lo, hi) g in (BoundedInt a, g')

  random g = let (a, g') = random g in (BoundedInt a, g')

instance Arbitrary BoundedInt where
  arbitrary = BoundedInt <$> choose (0, 100)

instance Arbitrary GameConfig where
  arbitrary = do
    w  <- unBoundedInt <$> arbitrary `suchThat` (> 0)
    h  <- unBoundedInt <$> arbitrary `suchThat` (> 0)
    sp <- unBoundedInt <$> arbitrary
    return $ GameConfig w h sp 1

instance Arbitrary Player where
  arbitrary = do
    i <- PlayerId <$> arbitrary
    n <- PlayerNick . fromString <$> arbitrary
    x <- unBoundedInt <$> arbitrary
    y <- unBoundedInt <$> arbitrary
    o <- arbitraryBoundedEnum
    return $ Player i n Alive (x, y) o []

genPlayer :: GameConfig -> Gen Player
genPlayer GameConfig{..} = do
  pid  <- PlayerId <$> arbitrary
  nick <- PlayerNick . fromString <$> arbitrary
  x    <- unBoundedInt <$> arbitrary `suchThat` (>= 0) `suchThat` (< BoundedInt gameWidth)
  y    <- unBoundedInt <$> arbitrary `suchThat` (>= 0) `suchThat` (< BoundedInt gameHeight)
  o    <- arbitraryBoundedEnum
  return $ Player pid nick Alive (x, y) o []

instance Arbitrary Game where
  arbitrary = do
    conf   <- arbitrary
    np     <- unBoundedInt
              <$> arbitrary `suchThat` (< (BoundedInt $ gameWidth conf * gameHeight conf))
                            `suchThat` (> 0)
    ps     <- nubBy areOverlappingPlayers
              <$> vectorOf np (genPlayer conf)
    let mp = Map.fromList . map (\p -> (playerId p, p)) $ ps
    return $ Game Nothing mp InProgress conf

isValidPlayer :: GameConfig -> Player -> Bool
isValidPlayer (GameConfig w h _ _) (Player _ _ _ (x, y) _ _) = x < w && y < h

areOverlappingPlayers :: Player -> Player -> Bool
areOverlappingPlayers p1 p2 =
  playerNick p1 == playerNick p2 || playerCoordinate p1 == playerCoordinate p2

isPlayerOnGrid :: Game -> Player -> Bool
isPlayerOnGrid game = isValidPlayer (gameConfig game)

