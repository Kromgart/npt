module NPT.Gemini.Client where

import qualified NPT.Gemini.Internal as GI
import qualified NPT.Gemini.Types as T
import qualified Servant.Client as SC
import Control.Monad.Trans.Except (ExceptT (ExceptT), withExceptT)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import NPT.Gemini.Configuration (GeminiSettings, host, port)
import Servant.API ((:<|>) ((:<|>)))
import Data.Proxy (Proxy (Proxy))

api :: Proxy T.ProjectsApi
api = Proxy

prjInfoClient :: SC.ClientM [T.ProjectInfo]

timeReportsClient :: T.TimeReportRequest -> SC.ClientM  [T.TimeReport]
 
prjInfoClient :<|> timeReportsClient = SC.client api

runRequest :: GeminiSettings -> SC.ClientM a -> GI.RequestResult a
runRequest cfg c = withExceptT GI.SrvError $ ExceptT $ do manager <- newManager defaultManagerSettings
                                                          SC.runClientM c (SC.ClientEnv manager (SC.BaseUrl SC.Http gmnHost gmnPort ""))
  where gmnHost = host cfg
        gmnPort = port cfg

getProjects :: GeminiSettings -> GI.RequestResult [T.ProjectInfo]
getProjects cfg = runRequest cfg prjInfoClient

getTimeReports :: GeminiSettings -> T.TimeReportRequest -> GI.RequestResult [T.TimeReport]
getTimeReports cfg t = runRequest cfg (timeReportsClient t)
