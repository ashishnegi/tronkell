{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module TestServer where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Data.ByteString.Char8 as C (pack)
import qualified Data.Map as M
import Control.Concurrent
import Control.Concurrent.STM

import Tronkell.Server.Types as STypes
import Tronkell.Server.Server as SServer
import Tronkell.Game.Types as GTypes


-- rethink the tests.
main :: IO ()
main = return ()
