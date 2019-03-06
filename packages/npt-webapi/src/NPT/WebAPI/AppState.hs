module NPT.WebAPI.AppState where

import qualified NPT.Data as DAT
import qualified NPT.Data.Project as PRJ 
import qualified NPT.Data.Resource as RES 

import Control.Concurrent.STM ( TVar
                              , newTVarIO
                              , readTVarIO
                              , writeTVar
                              , atomically
                              )

import Control.Monad.IO.Class (MonadIO, liftIO)

import Servant.Auth.Server (JWTSettings, defaultJWTSettings, generateKey)


data AppState = AppState { appStateVal1 :: String

                         , projectsCache :: TVar (Maybe [DAT.KeyedObject PRJ.Project])
                         , resourcesCache :: TVar (Maybe [DAT.KeyedObject RES.Resource])

                         , jwtSettings :: JWTSettings
                         }


mkAppState :: IO AppState
mkAppState = AppState "Boom" <$> newTVarIO Nothing
                             <*> newTVarIO Nothing
                             <*> (defaultJWTSettings <$> generateKey)



readThroughCache :: MonadIO m => TVar (Maybe a) -> m a -> m a
readThroughCache tvar geta = do cval <- liftIO $ readTVarIO tvar
                                case cval of (Just x) -> return x
                                             Nothing -> do a <- geta
                                                           liftIO $ atomically $ writeTVar tvar (Just a)
                                                           return a

clearingCache :: MonadIO m => TVar (Maybe a) -> m b -> m b
clearingCache tvar setb = (liftIO $ atomically $ writeTVar tvar Nothing) >> setb   -- just clearing given cache for now

