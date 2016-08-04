{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Control.Monad 
import Data.Aeson
import Data.Either
import Data.Maybe
import Data.Monoid
import Data.Text 
import System.IO.Unsafe
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import qualified Data.ByteString.Lazy as BS 

import Servant.API.SMSC.RU

data TestConfig = TestConfig Text Text Text

instance FromJSON TestConfig where 
  parseJSON (Object o) = TestConfig 
    <$> o .: "login"
    <*> o .: "password"
    <*> o .: "phone"
  parseJSON _ = mzero

testConfig :: TestConfig 
testConfig = fromJust $ decode' $ unsafePerformIO (BS.readFile "smsc.test.json")

-- | Load config from file 
testSmscConfig :: SmscConfig
testSmscConfig = (unsafePerformIO defaultSmscConfig) {
    smscLogin = login
  , smscPassword = pass
  }
  where 
  TestConfig login pass _ = testConfig

-- | Phone number we test with
testPhone :: Text 
testPhone = let TestConfig _ _ phone = testConfig in phone 

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcProperties, unitTests]

qcProperties :: TestTree 
qcProperties = testGroup "Properties" [ 
      testCase "Simple send" $ do
      res <- simpleSmsSend testSmscConfig testPhone "Test message"
      case res of 
        Left er -> assertFailure $ "message sending: " <> show er
        Right _ -> return ()
    , testCase "Simple cost" $ do 
      res <- getSimpleSmsCost testSmscConfig testPhone "Test message"
      case res of 
        Left er -> assertFailure $ "message costing: " <> show er
        Right v -> putStrLn $ "message cost: " <> show v
  ]

unitTests :: TestTree 
unitTests = testGroup "Unit tests" [

  ]