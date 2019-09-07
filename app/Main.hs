{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Persistence.Db
import           Persistence.EventLog
import           Persistence.TMFrame
import           RIO
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Modifiers
import           System.Random
import Control.Monad
import Control.Monad.Trans
import Conduit
import Prelude

data Logging a = Logging { appLogFunc :: LogFunc, applicationCtx :: a }

instance HasLogFunc (Logging a) where
    logFuncL :: Lens' (Logging a) LogFunc
    logFuncL d (Logging x a) = (\x -> Logging x a) <$> d x

testApp :: RIO (Logging ()) ()
testApp = do
    logInfo "Starting 1"
    logWarn "Starting 1"
    logError "Starting 1"

main = do
    logOptions <- logOptionsHandle stderr True
    (termLog, killTL) <- newLogFunc logOptions
    (logDB, killDB) <- logToSQLiteDatabase "event.db"
    ((storeFrame , retrieveFrames), killFrameDB) <- accessTMFrameDatabase  "frames.db"
    testFrameApp storeFrame retrieveFrames
    runRIO (Logging termLog ()) $ prependLogger logDB testApp
    killDB
    killTL
    killFrameDB

generateFrame :: Gen TMFrame
generateFrame = do
    NonNegative d <- arbitrary 
    PrintableString v <- arbitrary
    NonNegative scid <- arbitrary 
    NonNegative vcid <- arbitrary 
    NonNegative mcfc <- arbitrary 
    NonNegative vcfc <- arbitrary 
    pure $ 
        TMFrame d (fromString v) scid vcid mcfc vcfc  

testFrameApp store retrieve = do
    xs <- fmap concat  $ replicateM 100 $  sample' generateFrame
    store xs
    runConduit $ retrieve TMQueryAll .| (awaitForever $ liftIO . print)


