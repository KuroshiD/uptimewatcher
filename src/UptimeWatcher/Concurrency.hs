module UptimeWatcher.Concurrency (parallelMonitor) where

import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent.STM (TVar, readTVarIO)
import Network.HTTP.Client (Manager)
import UptimeWatcher.Monitor (checkAndReport)

parallelMonitor :: TVar [String] -> TVar [(String, Int)] -> Manager -> IO ()
parallelMonitor urls alerts manager = do
  urlList <- readTVarIO urls
  mapConcurrently_ (\url -> checkAndReport alerts manager url) urlList
