{-# LANGUAGE OverloadedStrings #-}

module NPT.BambooHR.Import.Timeoffs where

import NPT.BambooHR.Internal ( ConversionError (ConversionFieldError, ConversionOtherError)
                             , IdsToExtMap
                             , FieldError
                             , rdReq
                             , mapLeft
                             )


import qualified NPT.BambooHR.Types as T
import qualified NPT.Data.Timeoff as TOF
import qualified NPT.Data.Resource as RES

import Data.Int (Int64)
import Data.List (find)
import Data.Time.Clock (UTCTime (UTCTime))
import Data.Time.LocalTime ( midnight
                           , timeOfDayToTime)


type ResIdExtMap = IdsToExtMap RES.Resource


convertToTimeoff :: ResIdExtMap -> T.TimeoffEntry -> Either (ConversionError T.TimeoffEntry) TOF.Timeoff
convertToTimeoff idsMap t = do tid <- ferr $ fmap Just $ rdReq "TimeoffId" $ T.timeoffId t
                               empId <- ferr $ rdReq "EmployeeId" $ T.employeeId t
                               resId <- getResId empId
                               sDate <- ferr $ rdReq "StartDate" $ T.startDate t
                               eDate <- ferr $ rdReq "EndDate" $ T.endDate t
                               lastUpd <- (UTCTime <$> (ferr $ rdReq "LastChanged" $ T.lastChanged t)
                                                   <*> (pure $ timeOfDayToTime midnight))
                               
                               let tofType = TOF.Vacation -- TODO
                               let tofStatus = TOF.Approved -- TODO

                               mapLeft (ConversionOtherError t) $ TOF.mkTimeoff tid resId sDate eDate lastUpd tofType tofStatus

  where ferr :: Either FieldError a -> Either (ConversionError T.TimeoffEntry) a
        ferr = mapLeft (ConversionFieldError t)

        getResId :: Int64 -> Either (ConversionError T.TimeoffEntry) RES.ResourceId
        getResId x = case resId' of Nothing -> Left $ ConversionOtherError t $ "Error: cannot find a matching resource for id " ++ show x
                                    (Just (z, _)) -> Right z
          where resId' :: Maybe (RES.ResourceId, Int64)
                resId' = find ((== x) . snd) idsMap





         
