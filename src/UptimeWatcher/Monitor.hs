module UptimeWatcher.Monitor (monitorURLs, checkAndReport) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', readTVarIO)
import UptimeWatcher.HTTPClient (checkURL)
import Network.HTTP.Client (Manager)

monitorURLs :: TVar [String] -> TVar [(String, Int)] -> Manager -> IO [(String, String)]
monitorURLs urls alerts manager = do
  urlList <- readTVarIO urls
  statuses <- mapM (checkAndReport alerts manager) urlList
  return statuses

checkAndReport :: TVar [(String, Int)] -> Manager -> String -> IO (String, String)
checkAndReport alerts manager url = do
  isOnline <- checkURL manager url
  if isOnline
    then do
      atomically $ modifyTVar' alerts (filter (\(u, _) -> u /= url)) 
      putStrLn $ url ++ " is online."
      return (url, "online")
    else do
      atomically $ modifyTVar' alerts (incrementAlert url)
      putStrLn $ url ++ " is offline."
      return (url, "offline")

incrementAlert :: String -> [(String, Int)] -> [(String, Int)]
incrementAlert url alerts =
  let updated = map (\(u, count) -> if u == url then (u, count + 1) else (u, count)) alerts
  in if any (\(u, _) -> u == url) alerts then updated else (url, 1) : alerts
