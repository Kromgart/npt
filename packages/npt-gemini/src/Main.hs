{-# LANGUAGE Arrows #-}

module Main where

import qualified NPT.Gemini.Configuration as CFG
import qualified NPT.Gemini.Internal as GI
import qualified NPT.Gemini.Types as T
import NPT.Gemini.Import.ArgsHandler (getTimeParameter)
import NPT.Gemini.Import.TimeReports (getTimeReports)
import NPT.Gemini.Import.Projects ( ProjectIdExtMap
                                  , ProjectTypeIdExtMap
                                  , convertToProject
                                  , getGeminiProjects
                                  )

import qualified NPT.Data as DAT
import qualified NPT.Data.LoggedTime as LGT
import qualified NPT.Data.Resource as RES
import qualified NPT.Data.Project as PRJ
import qualified NPT.Data.ProjectType as PT



import Control.Arrow (returnA)
import Control.Monad.Trans.Except ( ExceptT (ExceptT)
                                  , withExceptT
                                  , runExceptT
                                  )
import Data.Either (partitionEithers)
import Data.List (nub, (\\))
import Data.Text (Text)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Clock (UTCTime, getCurrentTime)

import System.Exit (exitFailure)
import System.IO (appendFile)



main :: IO ()
main = do logMsg "Starting gemini import"
          res <- runExceptT execResult
          case res of (Left e) -> (logMsg $"Gemini import error: " ++ (show e)) >> exitFailure
                      (Right x) -> logMsg $ "Gemini import finished successfully: " ++ (show x)
           
  where logFile = "geminiimport.log"
        logMsg :: String -> IO ()
        logMsg msg = do t <- getCurrentTime
                        appendFile logFile $ (show t) ++ ": " ++ msg ++ "\n"
                        putStrLn msg  

        configName = "gemini-import.config.json"
        execResult :: GI.ImportResult ([GI.ConversionError T.ProjectInfo], [GI.ConversionError T.TimeReport])
        execResult = do cfg <- withExceptT GI.OtherError $ ExceptT $ CFG.readConfigFile configName
                        timeParams <- getTimeParameter cfg
                        importProjAndTimeLogsTrans cfg timeParams



importProjAndTimeLogsTrans :: CFG.Config -> UTCTime -> GI.ImportResult ([GI.ConversionError T.ProjectInfo], [GI.ConversionError T.TimeReport])
importProjAndTimeLogsTrans cfg tstmp = do let stringTimestamp = formatTime defaultTimeLocale "%D %T" tstmp
                                          prj <- getGeminiProjects gmnCfg
                                          res <- resEmails
                                          tr <- getTimeReports gmnCfg $ T.TimeReportRequest stringTimestamp (fst <$> res)
                                          withExceptT GI.DBError $ DAT.runTransQ dbcon importAllTQ2 (prj, tr, res)

  where dbcon = DAT.mkConnectionString $ CFG.dbSettings cfg
        gmnCfg = CFG.geminiSettings cfg

        resEmails :: GI.ImportResult [(Text, RES.ResourceId)]
        resEmails = withExceptT GI.DBError $ DAT.runSingleQ dbcon RES.getResourceEmailsQ ()




type ImportInputs = ([T.ProjectInfo], [T.TimeReport], [(Text, RES.ResourceId)])

type ImportOutputs = ([GI.ConversionError T.ProjectInfo], [GI.ConversionError T.TimeReport])



importAllTQ2 :: DAT.TransQuery ImportInputs ImportOutputs
importAllTQ2 = proc (prjs, trps, mails) -> do projTypes <- importProjTypesQ -< prjs
                                              (prjIds, prjErr) <- importProjectsQ -< (projTypes, prjs)
                                              trpsErr <- importTimelogsQ -< (trps, prjIds, mails)
                                              returnA -< (prjErr, trpsErr)


importProjTypesQ :: DAT.TransQuery [T.ProjectInfo] ProjectTypeIdExtMap
importProjTypesQ = proc pis -> do tsold <- DAT.TransQuery PT.getAllQ -< ()
                                  let ts = nub $ T.projectType <$> pis
                                      ts' = ts \\ fmap (PT.displayName . snd) tsold
                                  tsnew <- DAT.TransQuery $ DAT.mkBatchA PT.importQ -< PT.ProjectType <$> ts'
                                  returnA -< (fmap . fmap) PT.displayName (tsold ++ tsnew)


importProjectsQ :: DAT.TransQuery (ProjectTypeIdExtMap, [T.ProjectInfo]) (ProjectIdExtMap, [GI.ConversionError T.ProjectInfo])
importProjectsQ = proc (pts, pis) -> do let (convErr, projs) = partitionEithers $ convertToProject pts <$> pis
                                        ids <- DAT.TransQuery $ DAT.mkBatchA PRJ.importQ -< projs
                                        returnA -< (ids, convErr)
                                        

importTimelogsQ :: DAT.TransQuery ([T.TimeReport], ProjectIdExtMap, [(Text, RES.ResourceId)]) [GI.ConversionError T.TimeReport]
importTimelogsQ = proc (logs, pids, rids) -> do let (convErr, trs) = partitionEithers $ convertToLoggedTime pids rids <$> logs
                                                DAT.TransQuery (DAT.mkBatchA LGT.importLoggedTimeQ) -< trs
                                                returnA -< convErr

  where convertToLoggedTime :: ProjectIdExtMap -> [(Text, RES.ResourceId)] -> T.TimeReport -> Either (GI.ConversionError T.TimeReport) LGT.LoggedTime
        convertToLoggedTime pids emails report = do a <- GI.withError "Resource not found" report $ lookup (T.emailAddress report) emails
                                                    b <- GI.withError "Project not found" report $ lookup (T.project report) pids
                                                    return $ LGT.LoggedTime (Just $ T.reportId report) a b (T.entryDate report) (T.loggedTime report)





