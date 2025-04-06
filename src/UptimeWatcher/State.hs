module UptimeWatcher.State (addURL, removeURL, getURLs) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', readTVar)

type URL = String

addURL :: TVar [URL] -> URL -> IO ()
addURL urls url = atomically $ modifyTVar' urls (\us -> if url `elem` us then us else url : us)

removeURL :: TVar [URL] -> URL -> IO ()
removeURL urls url = atomically $ modifyTVar' urls (filter (/= url))

getURLs :: TVar [URL] -> IO [URL]
getURLs urls = atomically $ readTVar urls
