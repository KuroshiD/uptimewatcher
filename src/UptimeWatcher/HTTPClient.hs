module UptimeWatcher.HTTPClient (checkURL, createTLSManager) where

import Network.HTTP.Client (Manager, parseRequest, httpNoBody, responseStatus, HttpException)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (status200)
import Control.Exception (catch)

createTLSManager :: IO Manager
createTLSManager = newTlsManager

checkURL :: Manager -> String -> IO Bool
checkURL manager url = do
  catch (do
    request <- parseRequest url
    response <- httpNoBody request manager
    return $ responseStatus response == status200)
    handleException
  where
    handleException :: HttpException -> IO Bool
    handleException _ = return False
