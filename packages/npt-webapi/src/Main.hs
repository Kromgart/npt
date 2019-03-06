module Main where


import qualified NPT.WebAPI.Types as T
import qualified NPT.WebAPI.AppState as ST
import qualified NPT.WebAPI.Configuration as CFG
import qualified NPT.WebAPI.Server as SRV


import Data.Proxy (Proxy (Proxy))
import Data.Time.Clock (getCurrentTime)
import System.Directory (getCurrentDirectory)
import System.IO (appendFile)
import System.Exit (exitFailure)

import Network.HTTP.Types (hContentType, methodDelete)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, corsRequestHeaders, corsMethods)

import Servant ( serveWithContext
               , Context (EmptyContext, (:.))
               )
import Servant.Auth.Server (defaultCookieSettings)



theApp :: ST.AppState -> CFG.Config -> Application
theApp appState cfg = serveWithContext (Proxy :: Proxy T.NPTAPI)
                                       (ST.jwtSettings appState :. defaultCookieSettings :. EmptyContext) 
                                       (SRV.mkServer appState cfg)


main :: IO ()
main = do p <- getCurrentDirectory 
          logMsg $ "Starting. Working directory is " ++ p
          appState <- ST.mkAppState
          logMsg $ "Loading config file " ++ configName
          ecfg <- CFG.readConfigFile "npt.config.json"
          case ecfg of (Left e) -> exitWithError $ "Cannot read configuration: " ++ e
                       (Right cfg) -> do logMsg "Configuration loaded"
                                         let appPort = CFG.serverPort cfg
                                         let cpol = simpleCorsResourcePolicy { corsMethods = [methodDelete]
                                                                             , corsRequestHeaders = [hContentType] }
                                         logMsg $ "Opening port " ++ show appPort ++ " for incoming connections..."
                                         run appPort $ cors (const $ Just cpol) $ theApp appState cfg
          logMsg "Shutting down"

  where logFile = "npt-webapi.log"
        logMsg :: String -> IO ()
        logMsg msg = do t <- getCurrentTime
                        appendFile logFile $ (show t) ++ ": " ++ msg ++ "\n"
                        putStrLn msg


        exitWithError :: String -> IO ()
        exitWithError msg = (logMsg $ "Error: " ++ msg) >> exitFailure

        configName = "npt.config.json"


      



