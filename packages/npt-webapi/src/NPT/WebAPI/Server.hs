{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}


module NPT.WebAPI.Server where


import qualified NPT.WebAPI.Types as T
import qualified NPT.WebAPI.AppState as ST
import qualified NPT.WebAPI.Configuration as CFG
import qualified NPT.WebAPI.APIServer as ASR


import Servant ( (:<|>) ((:<|>))
               , Server
               , throwError
               , err301
               , errHeaders
               , serveDirectoryWebApp
               )
                   
import Network.HTTP.Types.Header (hLocation)




mkServer :: ST.AppState -> CFG.Config -> Server T.NPTAPI 
mkServer appState cfg = ASR.mkAPIServer appState cfg :<|> srvhome :<|> srvwebsite
  where srvhome :: Server T.NPTHomePage
        srvhome = throwError $ err301 { errHeaders = [(hLocation, "index.html")] }

        srvwebsite :: Server T.NPTWebsite
        srvwebsite = serveDirectoryWebApp "frontend"


