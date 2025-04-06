module Main (main) where

import Control.Concurrent (threadDelay, forkIO, killThread, ThreadId) -- Removido `myThreadId`
import Control.Concurrent.STM (TVar, newTVarIO, atomically, readTVarIO, writeTVar) -- Removido `modifyTVar'`
import Network.HTTP.Client (Manager)
import UptimeWatcher.State (addURL, removeURL) -- Removido `getURLs`
import UptimeWatcher.Monitor (monitorURLs)
import UptimeWatcher.HTTPClient (checkURL, createTLSManager)
import System.IO (hFlush, stdout)
import System.Console.ANSI (clearScreen)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

main :: IO ()
main = do
  putStrLn "Starting UptimeWatcher..."
  urls <- newTVarIO []
  alerts <- newTVarIO [] 
  manager <- createTLSManager 
  delay <- newTVarIO (30 * 60 * 1000000) 
  autoScanThread <- newTVarIO Nothing 

  forkAutoScan urls alerts manager delay autoScanThread

  menu urls alerts manager delay autoScanThread

menu :: TVar [String] -> TVar [(String, Int)] -> Manager -> TVar Int -> TVar (Maybe ThreadId) -> IO ()
menu urls alerts manager delay autoScanThread = do
  putStrLn "\nUptimeWatcher Menu:"
  putStrLn "1. Add URL"
  putStrLn "2. Remove URL"
  putStrLn "3. List URLs"
  putStrLn "4. Run manual scan"
  putStrLn "5. Scan specific URL"
  putStrLn "6. Set scan interval (in minutes)"
  putStrLn "7. Clear screen"
  putStrLn "8. Exit"
  putStr "Choose an option: "
  hFlush stdout
  option <- getLine
  case option of
    "1" -> do
      putStr "Enter URL to add: "
      hFlush stdout
      url <- getLine
      addURL urls url
      putStrLn "URL added."
      menu urls alerts manager delay autoScanThread
    "2" -> do
      putStr "Enter URL to remove: "
      hFlush stdout
      url <- getLine
      removeURL urls url
      putStrLn "URL removed."
      menu urls alerts manager delay autoScanThread
    "3" -> do
      currentUrls <- readTVarIO urls
      putStrLn "Monitored URLs:"
      mapM_ putStrLn currentUrls
      menu urls alerts manager delay autoScanThread
    "4" -> do
      putStrLn "Running manual scan..."
      currentTime <- getCurrentTime
      let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
      statuses <- monitorURLs urls alerts manager
      logResults timestamp statuses alerts 
      putStrLn "Scan completed."
      menu urls alerts manager delay autoScanThread
    "5" -> do
      putStr "Enter URL to scan: "
      hFlush stdout
      url <- getLine
      putStrLn $ "Scanning " ++ url ++ "..."
      isOnline <- checkURL manager url
      if isOnline
        then putStrLn $ url ++ " is online."
        else putStrLn $ url ++ " is offline."
      menu urls alerts manager delay autoScanThread
    "6" -> do
      currentDelay <- readTVarIO delay
      putStrLn $ "Current scan interval: " ++ show (currentDelay `div` (60 * 1000000)) ++ " minutes"
      putStr "Enter new scan interval in minutes: "
      hFlush stdout
      minutes <- getLine
      let microseconds = read minutes * 60 * 1000000
      atomically $ writeTVar delay microseconds
      putStrLn "Scan interval updated. Restarting timer..."
      restartAutoScan urls alerts manager delay autoScanThread
      menu urls alerts manager delay autoScanThread
    "7" -> do
      clearScreen
      menu urls alerts manager delay autoScanThread
    "8" -> putStrLn "Exiting UptimeWatcher. Goodbye!"
    _   -> do
      putStrLn "Invalid option. Try again."
      menu urls alerts manager delay autoScanThread

forkAutoScan :: TVar [String] -> TVar [(String, Int)] -> Manager -> TVar Int -> TVar (Maybe ThreadId) -> IO ()
forkAutoScan urls alerts manager delay autoScanThread = do
  threadId <- forkIO $ autoScan urls alerts manager delay 
  atomically $ writeTVar autoScanThread (Just threadId)

restartAutoScan :: TVar [String] -> TVar [(String, Int)] -> Manager -> TVar Int -> TVar (Maybe ThreadId) -> IO ()
restartAutoScan urls alerts manager delay autoScanThread = do
  maybeThreadId <- readTVarIO autoScanThread
  case maybeThreadId of
    Just threadId -> killThread threadId 
    Nothing -> return ()
  forkAutoScan urls alerts manager delay autoScanThread 

autoScan :: TVar [String] -> TVar [(String, Int)] -> Manager -> TVar Int -> IO ()
autoScan urls alerts manager delay = do
  interval <- readTVarIO delay
  threadDelay interval 
  loop
  where
    loop = do
      clearScreen
      currentTime <- getCurrentTime
      let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
      putStrLn $ "Starting automatic scan at " ++ timestamp
      statuses <- monitorURLs urls alerts manager 
      logResults timestamp statuses alerts
      interval <- readTVarIO delay
      threadDelay interval
      loop

logResults :: String -> [(String, String)] -> TVar [(String, Int)] -> IO ()
logResults timestamp statuses alerts = do
  alertCounts <- readTVarIO alerts
  appendFile "scan.log" $ "Scan at " ++ timestamp ++ ":\n"
  appendFile "scan.log" $ "Monitored URLs:\n"
  mapM_ (\(url, status) -> appendFile "scan.log" $ url ++ " " ++ status ++ "\n") statuses
  appendFile "scan.log" $ "Alerts:\n"
  mapM_ (\(url, count) -> appendFile "scan.log" $ url ++ " IS OFFLINE IN " ++ show count ++ " CONSECUTIVE SCANS\n") alertCounts
  appendFile "scan.log" $ "-----------------------------\n"
