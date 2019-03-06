{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}


module NPT.WebAPI.APIServer where


import qualified NPT.Data as DAT
import qualified NPT.WebAPI.Types as T
import qualified NPT.WebAPI.AuthTypes as AUTH
import qualified NPT.WebAPI.AppState as ST
import qualified NPT.WebAPI.Configuration as CFG

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except ( ExceptT (ExceptT)
                                  , withExceptT
                                  , throwE
                                  )

import Control.Monad.Trans.Reader ( ReaderT (ReaderT)
                                  , runReaderT
                                  , ask
                                  )


import Data.Text.Lazy (pack, toStrict)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import Data.Time.Calendar (Day)

import Servant ( (:<|>) ((:<|>))
               , hoistServerWithContext
               , Proxy (Proxy)
               , Server
               , ServerT
               , ServantErr
               , err401
               , err403
               , err500
               , errBody
               , Handler (Handler)
               )

import Servant.Auth.Server ( makeJWT
                           , AuthResult (Authenticated)
                           , JWTSettings
                           , CookieSettings
                           , throwAll
                           )


toServErr :: Functor m => ExceptT DAT.DataError m a -> ExceptT ServantErr m a
toServErr = withExceptT f
  where f (DAT.AuthenticationError e) = err401 { errBody = (encodeUtf8 $ pack $ "Authentication failed: " ++ e) }
        f (DAT.AuthorizationError e)  = err403 { errBody = (encodeUtf8 $ pack $ "Not authorized: " ++ e) }
        f e = err500 { errBody = (encodeUtf8 $ pack $ show e) }
  

toDateRange :: Day -> Day -> DAT.DBResult DAT.DateRange
toDateRange d1 d2 = case DAT.mkDateRange d1 d2 of (Left e) -> throwE $ DAT.OtherError e
                                                  (Right x) -> return x


p1 :: Proxy T.NPTWebAPI
p1 = Proxy

p2 :: Proxy '[JWTSettings, CookieSettings]
p2 = Proxy


mkAPIServer :: ST.AppState -> CFG.Config -> Server T.NPTWebAPI 
mkAPIServer appState cfg = hoistServerWithContext p1 p2 toHandler serverWebAPI
  where toHandler :: (ReaderT ST.AppState (ExceptT ServantErr IO) a) -> Handler a
        toHandler rdr = Handler $ runReaderT rdr appState

        conString :: DAT.ConnectionString
        conString = DAT.mkConnectionString $ CFG.dbSettings cfg

        withDB :: (DAT.ConnectionString -> a) -> a
        withDB f = f conString


        serverWebAPI :: ServerT T.NPTWebAPI (ReaderT ST.AppState (ExceptT ServantErr IO))
        serverWebAPI = srvLogin :<|> protectedWebAPI


        srvLogin (AUTH.Creds u p) = do state <- ask
                                       lift $ toServErr $ do (uid, _) <- withDB DAT.loginUser u p
                                                             t <- withExceptT (const $ DAT.OtherError "Login failed") $ ExceptT $ makeJWT uid (ST.jwtSettings state) Nothing
                                                             return $ toStrict $ decodeUtf8 t


        protectedWebAPI :: AuthResult AUTH.AuthTokenData -> ServerT T.RequiresLoginAPI (ReaderT ST.AppState (ExceptT ServantErr IO))
        --protectedWebAPI (Authenticated _) = srvRes 
        protectedWebAPI _ =                 srvRes 
                                       :<|> srvProj 
                                       :<|> srvProjTypes
                                       :<|> srvGrpTypes
                                       :<|> srvGroups
                                       :<|> srvWOrders 
                                       :<|> srvBookings 
                                       :<|> srvTables

        -- protectedWebAPI _ = throwAll $ err401 { errBody = encodeUtf8 "Authentication failed" }

        srvRes = do state <- ask
                    lift $ ST.readThroughCache (ST.resourcesCache state) $ toServErr $ withDB DAT.getResources

        srvProj = srvProjGet :<|> srvProjIns
          where srvProjGet = do state <- ask 
                                lift $ ST.readThroughCache (ST.projectsCache state) $ toServErr $ withDB DAT.getProjects

                srvProjIns x = do state <- ask
                                  lift $ ST.clearingCache (ST.projectsCache state) $ toServErr $ withDB DAT.insertProject x


        srvProjTypes = lift $ toServErr $ withDB DAT.getProjectTypes


        srvGrpTypes = lift $ toServErr $ withDB DAT.getGroupTypes


        srvGroups = lift $ toServErr $ withDB DAT.getGroups



        srvWOrders = srvWOrdersGet :<|> srvWOrdersIns :<|> srvWOrdersUpd :<|> srvWOrdersDel
          where srvWOrdersGet     = lift $ toServErr $ withDB DAT.getWorkOrders
                srvWOrdersIns x   = lift $ toServErr $ withDB DAT.insertWorkOrder x
                srvWOrdersUpd i x = lift $ toServErr $ withDB DAT.updateWorkOrder i x
                srvWOrdersDel i   = lift $ toServErr $ withDB DAT.deleteWorkOrder i


        srvBookings = srvBookingsGet :<|> srvBookingUpd :<|> srvBookingDel :<|> srvBookingIns
          where srvBookingsGet d1 d2 = lift $ toServErr $ toDateRange d1 d2 >>= withDB DAT.getAllBookingsInRange
                srvBookingUpd i x    = lift $ toServErr $ withDB DAT.updateBooking i x 
                srvBookingDel i      = lift $ toServErr $ withDB DAT.deleteBooking i 
                srvBookingIns x      = lift $ toServErr $ withDB DAT.insertBooking x

        srvTables = srvTablesRes :<|> srvTablesWorkOrders :<|> srvTablesWorkOrdersExec
          where srvTablesRes d1 d2 = ReaderT $ tbls 
                  where tbls _ = toServErr $ toDateRange d1 d2 >>= withDB DAT.getResourcePlanningTable

                srvTablesWorkOrders d1 d2 = ReaderT $ tbls
                  where tbls _ = toServErr $ toDateRange d1 d2 >>= withDB DAT.getWorkOrdersTable

                srvTablesWorkOrdersExec d1 d2 grps = ReaderT $ tbls
                  where tbls _ = toServErr $ do dr <- toDateRange d1 d2 
                                                withDB DAT.getWorkOrdersExecutiveTable dr grps



