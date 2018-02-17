module Reveille
  ( defaultMain
  ) where

import Reveille.Aggregator (startAggregator)
import Reveille.Database (initialDatabase)
import Reveille.Server (startServer)

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as Stm
import qualified Network.HTTP.Client.TLS as Client

defaultMain :: IO ()
defaultMain = do
  manager <- Client.newTlsManager
  database <- Stm.newTVarIO initialDatabase

  Async.concurrently_
    (startAggregator manager database)
    (startServer database)
