{-# LANGUAGE OverloadedStrings #-}

module NPT.BambooHR.Client where


import qualified NPT.BambooHR.Types as T
import qualified NPT.BambooHR.Configuration as CFG


import Control.Monad.Trans.Except (ExceptT (ExceptT), withExceptT ,throwE)
import Data.Proxy (Proxy (Proxy))
import Data.Time.Calendar (Day, showGregorian)
import qualified Data.Text as TXT


import Network.HTTP.Client.TLS (newTlsManager)
import qualified Servant.Client as CL


employeesAPI :: Proxy T.EmployeesAPI
employeesAPI = Proxy 


timeoffAPI :: Proxy T.TimeoffAPI
timeoffAPI = Proxy


data RequestError = InvalidArgs String
                  | SrvError CL.ServantError
                  deriving Show


type RequestResult a = ExceptT RequestError IO a


runRequest :: CFG.BambooSettings -> CL.ClientM a -> RequestResult a
runRequest cfg c = withExceptT SrvError $ ExceptT $ do mgr <- newTlsManager
                                                       CL.runClientM c $ CL.ClientEnv mgr baseUrl
  where baseUrl :: CL.BaseUrl
        baseUrl = CL.BaseUrl CL.Https (CFG.bbHostname cfg) 443 ("/api/gateway.php/" ++ (CFG.bbCompany cfg) ++ "/v1")


withStdHeaders :: T.AuthToken -> (Maybe T.AuthToken -> Maybe T.ContentType -> a) -> a
withStdHeaders t f = f (Just t) (Just "application/json")


getEmployees :: CFG.BambooSettings -> RequestResult [T.EmployeeDef]
getEmployees cfg = runRequest cfg g'
  where g' :: CL.ClientM [T.EmployeeDef]
        g' = fmap T.runList $ withStdHeaders (CFG.bbAuthToken cfg) (CL.client employeesAPI) rpr
        fld n = TXT.concat ["<field id=\"", n,  "\" />"]
        rpr = TXT.concat [ "<report><fields>"
                         , fld "id"
                         , fld "supervisorEId"
                         , fld "displayName"
                         , fld "hireDate"
                         , fld "terminationDate"
                         , fld "jobTitle"
                         , fld "location"
                         , fld "workEmail"
                         , fld "mobilePhone"
                         , "</fields></report>" ]


getTimeoffs :: CFG.BambooSettings -> Day -> Day -> RequestResult [T.TimeoffEntry]
getTimeoffs cfg d1 d2 | d1 < d2 = runRequest cfg g'
                      | otherwise = throwE $ InvalidArgs $ "Invalid date range: " ++ d1' ++ ", " ++ d2'
  where g' :: CL.ClientM [T.TimeoffEntry]
        g' = withStdHeaders (CFG.bbAuthToken cfg) (CL.client timeoffAPI) (Just d1') (Just d2') (Just "requested,approved") 
        d1' = showGregorian d1
        d2' = showGregorian d2
  

 

