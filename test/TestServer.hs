{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module TestServer where

import Test.Hspec
import Test.QuickCheck

import Tronkell.Server.Types as STypes
import Tronkell.Server.Server as SServer
import Tronkell.Game.Types as GTypes
import Tronkell.Utils as TUtils

import Data.List (nub, foldl')
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import qualified TestUtils as TU

import Debug.Trace

-- rethink the tests.
main :: IO ()
main = hspec $ do
  describe "User workflow" $ do
    it "should be able to join (ready) again after she goes to waiting" $
      property $ \ users' clientsPos -> do
        user <- arbitrary -- atleast one user
        let users = Map.insert (STypes.userId user) user users'
        exitMsgs <- sequence $ map (genUserMsg users . (Positive 1,)) clientsPos
        joinMsgs <- sequence $ map (genUserMsg users . (Positive 0,)) clientsPos -- 0 is Ready
        let finalUsers = foldl' (flip usersAfterMsg) users (exitMsgs ++ joinMsgs)
            userIndexes = map (flip mod (Map.size users) . getPositive) $ clientsPos
        return $ all (== Ready) $ fmap (\i -> userState . snd . Map.elemAt i $ finalUsers) $ userIndexes

    it "should not be able to join (ready) again after she exits => should not be in the users map" $
      property $ \ users' clientsPos -> do
        user <- arbitrary -- atleast one user
        let users = Map.insert (STypes.userId user) user users'
        exitMsgs <- sequence $ map (genUserMsg users . (Positive 2,)) clientsPos -- 2 means UserExit
        joinMsgs <- sequence $ map (genUserMsg users . (Positive 0,)) clientsPos
        let finalUsers = foldl' (flip usersAfterMsg) users (exitMsgs ++ joinMsgs)
            userIds = catMaybes . map userMsgToUserId $ (exitMsgs ++ joinMsgs)
        return $ null . catMaybes . fmap (\ userId -> Map.lookupIndex userId finalUsers) $ userIds

  where
    genUserMsg :: Users -> (Positive Int, Positive Int) -> Gen UserMessage
    genUserMsg users (Positive eventNo, Positive clientNo) =
      let userId = Map.keys users !! (clientNo `mod` Map.size users)
      in case eventNo `mod` 5 of
        0 -> return $ UserInMessage (PlayerReady userId)
        1 -> return $ UserInMessage (PlayerExit userId)
        2 -> return $ UserInMessage (UserExit userId)
        3 -> do
          status <- arbitrary
          return $ MoveAllToStatus status
        4 -> do
          user <- arbitrary
          return $ NewUser user
        _ -> error "Should never happen"

    userMsgToUserId :: UserMessage -> Maybe UserID
    userMsgToUserId msg =
      case msg of
        UserInMessage inMsg -> Just . TUtils.getUserId $ inMsg
        NewUser user -> Just $ userId user
        _ -> Nothing
