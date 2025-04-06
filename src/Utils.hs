module Utils (logMessage) where

logMessage :: String -> IO ()
logMessage msg = putStrLn $ "[LOG] " ++ msg
