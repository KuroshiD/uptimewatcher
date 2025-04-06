module Main (main) where

import Control.Concurrent (threadDelay, forkIO, killThread, ThreadId) 
import Control.Concurrent.STM (TVar, newTVarIO, atomically, readTVarIO, writeTVar) 
import Network.HTTP.Client (Manager)
import UptimeWatcher.State (addURL, removeURL)
import UptimeWatcher.Monitor (monitorURLs)
import UptimeWatcher.HTTPClient (checkURL, createTLSManager)
import System.IO (hFlush, stdout)
import System.Console.ANSI (clearScreen)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

main :: IO ()
main = do
  putStrLn "Starting UptimeWatcher..."
  urls <- newTVarIO [] -- List of monitored URLs.
  alerts <- newTVarIO [] -- List of alerts for consecutive offline URLs.
  manager <- createTLSManager -- HTTP manager for requests.
  delay <- newTVarIO (30 * 60 * 1000000) -- Default interval of 30 minutes in microseconds.
  autoScanThread <- newTVarIO Nothing -- Optional thread for automatic scanning.

  forkAutoScan urls alerts manager delay autoScanThread -- Starts automatic scanning.

  menu urls alerts manager delay autoScanThread -- Displays the main menu.

menu :: TVar [String] -> TVar [(String, Int)] -> Manager -> TVar Int -> TVar (Maybe ThreadId) -> IO ()
menu urls alerts manager delay autoScanThread = do
  -- Interactive menu for the user.
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
      -- Adds a URL to the monitored list.
      putStr "Enter URL to add: "
      hFlush stdout
      url <- getLine
      addURL urls url
      putStrLn "URL added."
      menu urls alerts manager delay autoScanThread
    "2" -> do
      -- Removes a URL from the monitored list.
      putStr "Enter URL to remove: "
      hFlush stdout
      url <- getLine
      removeURL urls url
      putStrLn "URL removed."
      menu urls alerts manager delay autoScanThread
    "3" -> do
      -- Lists all currently monitored URLs.
      currentUrls <- readTVarIO urls
      putStrLn "Monitored URLs:"
      mapM_ putStrLn currentUrls
      menu urls alerts manager delay autoScanThread
    "4" -> do
      -- Performs a manual scan of all URLs.
      putStrLn "Running manual scan..."
      currentTime <- getCurrentTime
      let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
      statuses <- monitorURLs urls alerts manager -- Checks the status of all URLs.
      logResults timestamp statuses alerts -- Logs the results.
      putStrLn "Scan completed."
      menu urls alerts manager delay autoScanThread
    "5" -> do
      -- Scans a specific URL.
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
      -- Updates the automatic scan interval.
      currentDelay <- readTVarIO delay
      putStrLn $ "Current scan interval: " ++ show (currentDelay `div` (60 * 1000000)) ++ " minutes"
      putStr "Enter new scan interval in minutes: "
      hFlush stdout
      minutes <- getLine
      let microseconds = read minutes * 60 * 1000000
      atomically $ writeTVar delay microseconds
      putStrLn "Scan interval updated. Restarting timer..."
      restartAutoScan urls alerts manager delay autoScanThread -- Restarts automatic scanning with the new interval.
      menu urls alerts manager delay autoScanThread
    "7" -> do
      -- Clears the console screen.
      clearScreen
      menu urls alerts manager delay autoScanThread
    "8" -> putStrLn "Exiting UptimeWatcher. Goodbye!"
    _   -> do
      -- Invalid option.
      putStrLn "Invalid option. Try again."
      menu urls alerts manager delay autoScanThread

forkAutoScan :: TVar [String] -> TVar [(String, Int)] -> Manager -> TVar Int -> TVar (Maybe ThreadId) -> IO ()
forkAutoScan urls alerts manager delay autoScanThread = do
  -- Creates a new thread for automatic scanning.
  threadId <- forkIO $ autoScan urls alerts manager delay 
  atomically $ writeTVar autoScanThread (Just threadId) -- Stores the thread ID.

restartAutoScan :: TVar [String] -> TVar [(String, Int)] -> Manager -> TVar Int -> TVar (Maybe ThreadId) -> IO ()
restartAutoScan urls alerts manager delay autoScanThread = do
  -- Restarts the automatic scanning thread, if it exists.
  maybeThreadId <- readTVarIO autoScanThread
  case maybeThreadId of
    Just threadId -> killThread threadId -- Terminates the existing thread.
    Nothing -> return ()
  forkAutoScan urls alerts manager delay autoScanThread -- Creates a new thread.

autoScan :: TVar [String] -> TVar [(String, Int)] -> Manager -> TVar Int -> IO ()
autoScan urls alerts manager delay = do
  -- Infinite loop for automatic scanning with a configurable interval.
  interval <- readTVarIO delay
  threadDelay interval -- Waits for the interval before starting the loop.
  loop
  where
    loop = do
      clearScreen
      currentTime <- getCurrentTime
      let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
      putStrLn $ "Starting automatic scan at " ++ timestamp
      statuses <- monitorURLs urls alerts manager -- Checks the status of URLs.
      logResults timestamp statuses alerts -- Logs the results.
      interval <- readTVarIO delay
      threadDelay interval -- Waits for the next interval.
      loop

logResults :: String -> [(String, String)] -> TVar [(String, Int)] -> IO ()
logResults timestamp statuses alerts = do
  -- Logs the scan results to a file.
  alertCounts <- readTVarIO alerts
  appendFile "scan.log" $ "Scan at " ++ timestamp ++ ":\n"
  appendFile "scan.log" $ "Monitored URLs:\n"
  mapM_ (\(url, status) -> appendFile "scan.log" $ url ++ " " ++ status ++ "\n") statuses
  appendFile "scan.log" $ "Alerts:\n"
  mapM_ (\(url, count) -> appendFile "scan.log" $ url ++ " IS OFFLINE IN " ++ show count ++ " CONSECUTIVE SCANS\n") alertCounts
  appendFile "scan.log" $ "-----------------------------\n"
