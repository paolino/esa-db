-- |
-- Module      :  Persistence.Db
-- Copyright   :  Paolo Veronelli, Matthias Putz
-- License     :  BSD3
--
-- Maintainer  :  paolo@global.de
-- Stability   :  experimental
-- Portability :  unknown
--
-- Database backend for Events
--


{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GADTs, OverloadedStrings, Rank2Types #-}


module Persistence.Db where

import           Control.Monad.State

import qualified Data.Text                       as T
import           Data.Time

import           Database.Selda
import           Database.Selda.Backend
import           Database.Selda.Backend.Internal
import           Database.Selda.SQLite
import           Database.Selda.SqlType

import           Persistence.EventLog
import           Persistence.TMFrame

import           RIO

import           System.Directory

import Data.Conduit

runQ :: MonadIO m => MVar (SeldaConnection b) -> SeldaT b m a -> m a
runQ cRef (S f) = do
    c <- takeMVar cRef
    (v, c') <- runStateT f c
    putMVar cRef c'
    pure v

data ATable = forall a. ATable (Table a)

createATable :: MonadSelda  m => ATable -> m ()
createATable (ATable x) = createTable x

newtype RunSelda = RunSelda (forall a. SeldaT SQLite IO a -> IO a)

accessSQLiteDatabase:: FilePath -> [ATable] -> IO (RunSelda, IO ())
accessSQLiteDatabase fp ts = do
    e <- doesFileExist fp
    cRef <- sqliteOpen fp >>= newMVar
    let run :: SeldaT SQLite IO a -> IO a
        run = runQ cRef
    when (not e) $ run $ mapM_ createATable ts 
    pure $ (RunSelda run , readMVar cRef >>= seldaClose)

customText :: Text -> Lit a
customText = LCustom TText . LText

customInt :: Integral a => a -> Lit a
customInt = LCustom TInt . LInt . fromIntegral

--------------------------------------------------------------------------------
-- LogLevel Row
--------------------------------------------------------------------------------

instance SqlType LogLevel where
    mkLit LevelDebug      = customText "debug"
    mkLit LevelInfo       = customText "info"
    mkLit LevelWarn       = customText "warn"
    mkLit LevelError      = customText "error"
    mkLit (LevelOther t)  = customText t
    sqlType _             = TText
    fromSql (SqlString x) = case x of
        "debug" -> LevelDebug
        "info"  -> LevelInfo
        "warn"  -> LevelWarn
        "error" -> LevelError
        y       -> LevelOther y
    defaultValue = customText "wtf"

instance SqlRow EventLog 

eventLogTable :: Table EventLog
eventLogTable = tableFieldMod "events_log" 
    [   #logTime :- index]
    (T.drop $ T.length "log")

logToSQLiteDatabase :: FilePath -> IO (EventLogger, IO ())
logToSQLiteDatabase fp = do
    (RunSelda run, kill) <- accessSQLiteDatabase fp [ATable eventLogTable]
    pure (run . insert_ eventLogTable . pure, kill)

--------------------------------------------------------------------------------
-- TMFrame Row
--------------------------------------------------------------------------------


instance SqlRow TMFrame

tMFrameTable :: Table TMFrame
tMFrameTable = tableFieldMod "tm_frames" 
    [ #frameTime :- index
    , #frameSCID :- index
    , #frameVCID :- index
    , #frameMCFC :- index
    , #frameVCFC :- index
    ]
    (T.drop $ T.length "frame")


instance SqlType Word8 where
    mkLit = customInt
    sqlType _             = TInt
    fromSql (SqlInt x) = fromIntegral x 
    defaultValue = customInt 0 

instance SqlType Word16 where
    mkLit = customInt
    sqlType _             = TInt
    fromSql (SqlInt x) = fromIntegral x 
    defaultValue = customInt 0 


window = 100

retrieve :: (forall a. SeldaT SQLite IO a -> IO a) -> TMQuery -> ConduitT () TMFrame IO ()
retrieve run TMQueryAll = let
    step n = do
        xs <- liftIO $ run $ query $ limit n window $ select tMFrameTable
        let l = length xs
        mapM_ yield xs
        case l >= window of 
            False -> return ()
            True -> step $ n + l
    in step 0

accessTMFrameDatabase :: FilePath -> IO ((TMFrameStore, TMFrameRetrieve), IO ())
accessTMFrameDatabase fp = do
    (RunSelda run, kill) <- accessSQLiteDatabase fp [ATable tMFrameTable]
    let     store = run . insert_ tMFrameTable  
    pure ((store, retrieve  run), kill)


