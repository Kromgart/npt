{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified NPT.BambooHR.Types as T
import qualified NPT.BambooHR.Client as CL
import qualified NPT.BambooHR.Import.Resources as IR
import qualified NPT.BambooHR.Import.Timeoffs as IT
import qualified NPT.BambooHR.Internal as H
import qualified NPT.BambooHR.Configuration as CFG

import qualified NPT.Data as DAT
import qualified NPT.Data.Timeoff as TOF
import qualified NPT.Data.Group as GRP
import qualified NPT.Data.GroupType as GRPT


import Control.Arrow (returnA)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT)
                                  , runExceptT
                                  , withExceptT
                                  )

import Control.Concurrent.Async ( Concurrently (Concurrently)
                                , runConcurrently)

import Data.Either (partitionEithers)
import Data.List (nub, (\\))
import Data.Maybe (catMaybes)
import Data.Time.Calendar ( Day
                          , fromGregorian)
import Data.Time.Clock (getCurrentTime)
import System.Exit (exitFailure)
import System.IO (appendFile)

import Data.Text (Text)

import Hasql.Query (Query)


main :: IO ()
main = do res <- runExceptT main''
          case res of (Left e) -> (logMsg $ "Error: " ++ show e) >> exitFailure
                      _ -> return ()

  where logFile = "bamboohr.log"
        logMsg :: String -> IO ()
        logMsg msg = do t <- getCurrentTime
                        appendFile logFile $ (show t) ++ ": " ++ msg ++ "\n"
                        putStrLn msg

        logMsgT = lift . logMsg                        

        configName = "bamboohr.config.json"

        main'' :: ExceptT H.ImportError IO ()
        main'' = do logMsgT $ "Loading config " ++ configName
                    cfg <- withExceptT H.OtherError $ ExceptT $ CFG.readConfigFile configName
                    logMsgT "Starting the import"
                    res <- importAllTrans cfg day1 day2
                    logMsgT $ "Import succeeded. Result: " ++ (show res)
  

day1 :: Day
day1 = fromGregorian 2018 1 1


day2 :: Day
day2 = fromGregorian 2018 2 28



type ImportInputs = ([DAT.KeyedObject GRP.Group], [T.EmployeeDef], [T.TimeoffEntry])

type ImportOutputs = ([H.ConversionError T.EmployeeDef], [H.ConversionError T.TimeoffEntry]) 


importAllTrans :: CFG.Config -> Day -> Day -> H.ImportResult ImportOutputs
importAllTrans cf d1 d2 = inputData >>= withExceptT H.DBError . DAT.runTransQ constr importAllTQ 
  where inputData :: H.ImportResult ImportInputs
        inputData = ExceptT $ runConcurrently ccRes
          where ccRes :: Concurrently (Either H.ImportError ImportInputs)
                ccRes = (,,) `fmap2` Concurrently (runExceptT $ withExceptT H.DBError $ DAT.getGroupsByType constr GRPT.locationGroupTypeId) 
                             `ap2`   Concurrently (runExceptT $ withExceptT H.BambooError $ CL.getEmployees bbCfg)
                             `ap2`   Concurrently (runExceptT $ withExceptT H.BambooError $ CL.getTimeoffs bbCfg d1 d2)

                fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
                fmap2 = fmap . fmap

                ap2 :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
                ap2 = (<*>) . fmap (<*>)

        bbCfg = CFG.bambooSettings cf
        constr = DAT.mkConnectionString $ CFG.dbSettings cf


importAllTQ :: DAT.TransQuery ImportInputs ImportOutputs
importAllTQ = proc (locs, emps, tents) -> do allLocs <- DAT.TransQuery importLocsQ -< (locs, emps)
                                             (resIds, resEr) <- importResTQ -< (emps, allLocs)
                                             tofEr <- DAT.TransQuery importTofsQ -< (tents, resIds)
                                             returnA -< (resEr, tofEr)


importLocsQ :: Query ([DAT.KeyedObject GRP.Group], [T.EmployeeDef]) ([DAT.KeyedObject GRP.Group])
importLocsQ = proc (grps, emps) -> do let extLocs :: [Text]
                                          extLocs = nub $ catMaybes $ T.empLocation <$> emps
                                          newLocs = extLocs \\ fmap (GRP.displayName . snd) grps
                                          newGrps = fmap ($ GRPT.locationGroupTypeId) $ GRP.Group <$> newLocs 
                                      newGrps' <- DAT.mkBatchA GRP.importQ -< newGrps
                                      returnA -< newGrps' ++ grps


importResTQ :: DAT.TransQuery ([T.EmployeeDef], [DAT.KeyedObject GRP.Group]) (IR.ResIdExtMap, [H.ConversionError T.EmployeeDef])
importResTQ = proc (emps, locGrps) -> do let (convErr, res) = partitionEithers $ IR.convertToResource locGrps <$> emps
                                         resIds <- IR.importResourcesTQ -< res
                                         returnA -< (resIds, convErr)


importTofsQ :: Query ([T.TimeoffEntry], IR.ResIdExtMap) ([H.ConversionError T.TimeoffEntry])
importTofsQ = proc (tofs, resIdsMap) -> do let (convErr, tofs') = partitionEithers $ IT.convertToTimeoff resIdsMap <$> tofs
                                           DAT.mkBatchA TOF.importQ -< tofs'
                                           returnA -< convErr



