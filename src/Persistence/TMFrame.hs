-- |
-- Module      :  Persistence.TMFrame
-- Copyright   :  Paolo Veronelli, Matthias Putz
-- License     :  BSD3
--
-- Maintainer  :  paolo@global.de
-- Stability   :  experimental
-- Portability :  unknown
--
-- Support RIO external logging
--

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}

module Persistence.TMFrame where

import           Control.Monad.Reader
import           Data.Text
import           Data.Time
import           RIO
import Data.Conduit

data TMFrame  = TMFrame 
    { frameTime    :: Int64 -- ^ earth arrival time 
    , frameContent   :: ByteString -- ^ raw data received
    , frameSCID :: Word16 -- ^ statellite id
    , frameVCID :: Word16 -- ^ 
    , frameMCFC :: Word8 -- ^ 
    , frameVCFC :: Word8 -- ^ 
    }
  deriving (Generic, Show)

type TMFrameStore = [TMFrame] -> IO ()

data TMQuery 
    = TMQueryAll 

type TMFrameRetrieve = TMQuery -> ConduitT () TMFrame IO ()


