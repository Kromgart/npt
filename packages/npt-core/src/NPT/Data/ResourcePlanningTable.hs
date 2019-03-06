{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}


module NPT.Data.ResourcePlanningTable where


import NPT.Data.Internal ( mkQuery
                         , addColName
                         , addColName3
                         , addTable
                         , ColumnName (ColumnName)
                         , DateRange (DateRange)
                         , dateRangeEncoder
                         )

import qualified NPT.Data.DataId as ID
import qualified NPT.Data.Resource as RES
import qualified NPT.Data.Project as PRJ
import qualified NPT.Data.Schedule as SCH
import qualified NPT.Data.Booking as BOK
import qualified NPT.Data.Timeoff as TOF


import Data.Functor.Contravariant (contramap)
import Data.Int (Int16)
import Data.List (foldl', find)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Calendar (Day)

import Control.Arrow (returnA)
import Control.Monad (replicateM, mfilter)

import GHC.Generics (Generic)

import Hasql.Query (Query)
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E





data PlanResRow = PlanResRow { resourceId :: RES.ResourceId
                             , booked :: [PlanResProjectRow]
                             , capacities :: [(Day, PlanResCapacity)]
                             }
                             deriving Generic



data PlanResProjectRow = PlanResProjectRow { projectId :: PRJ.ProjectId
                                           , bookings :: [PlanResBooking]
                                           }
                                           deriving Generic


data PlanResBooking = PlanResBooking { bookingId     :: BOK.BookingId
                                     , bookingDate   :: Day
                                     , bookingAmount :: Int16
                                     }
                                     deriving Generic


data PlanResCapacity = Some Int16
                     | NoneDueSchedule SCH.ScheduleId
                     | NoneDueTimeoff TOF.TimeoffId
                     | ScheduleMissing
                     | ResourceInactive Day (Maybe Day)
                     deriving Generic








queryTxt :: Text
queryTxt = "SELECT rs." `addColName3` RES.colPK
              <> ", rs." `addColName3` RES.colStart 
              <> ", rs." `addColName3` RES.colEnd

              <> ", sh." `addColName3` SCH.colPK
              <> ", sh." `addColName3` SCH.colStart
              <> ", sh." `addColName3` SCH.colCycle

              <> ", tb." `addColName3` BOK.colPK
              <> ", tb." `addColName3` BOK.colDate
              <> ", tb." `addColName3` BOK.colProjId
              <> ", tb." `addColName3` BOK.colAmount

              <> ", tb." `addColName3` TOF.colPK
              <> ", tb." `addColName` tofStart
              <> ", tb." `addColName` tofEnd

              <> " FROM " `addTable` RES.tblName <> " rs "
              <> " INNER JOIN " `addTable` SCH.tblName <> " sh ON (rs." `addColName3` RES.colSchId <> " = sh." `addColName3` SCH.colPK <> ")"
              <> " LEFT JOIN (" <> tbl2 <> ") tb ON (rs." `addColName3` RES.colPK <> " = tb." `addColName3` RES.colPK <> ")"

              <> " WHERE NOT (rs." `addColName3` RES.colStart <> " > $2 OR (rs." `addColName3` RES.colEnd <> " NOTNULL AND rs." `addColName3` RES.colEnd <> " < $1))"
              
              <> " ORDER BY rs." `addColName3` RES.colPK 
                      <> ", tb." `addColName3` PRJ.colPK

              <> ";"


               
  where tofStart = ColumnName "timeoff_start"
        tofEnd   = ColumnName "timeoff_end"
        csn x = "cast (NULL as " <> x <> ") " 
        castNull x n = (csn x) `addColName` n
        castNull3 x n = (csn x) `addColName3` n

        tbl2 :: Text
        tbl2 = "(" <> tblbok <> ") UNION (" <> tbltof <> ")"

   
        tblbok = "SELECT " <> "bok." `addColName3` BOK.colResId 

                           <> ", bok." `addColName3` BOK.colPK
                           <> ", bok." `addColName3` BOK.colDate
                           <> ", bok." `addColName3` BOK.colProjId
                           <> ", bok." `addColName3` BOK.colAmount

                           <> ", " <> castNull3 "bigint" TOF.colPK 
                           <> ", " <> castNull "date" tofStart
                           <> ", " <> castNull "date" tofEnd

                           <> " FROM " `addTable` BOK.tblName <> " bok"
                           <> " WHERE (" <> "bok." `addColName3` BOK.colDate <> " >= $1"
                                         <> " AND "
                                         <> "bok." `addColName3` BOK.colDate <> " <= $2"
                                  <> ")"



        tbltof = "SELECT " <> "tof." `addColName3` TOF.colResId 

                           <> ", NULL"
                           <> ", NULL"
                           <> ", NULL"
                           <> ", NULL"

                           <> ", tof." `addColName3` TOF.colPK 
                           <> ", tof." `addColName3` TOF.colStart
                           <> ", tof." `addColName3` TOF.colEnd

                           <> " FROM " `addTable` TOF.tblName <> " tof"
                           <> " WHERE NOT (" <> "tof." `addColName3` TOF.colStart <> " > $2"
                                             <> " OR "
                                             <> "tof." `addColName3` TOF.colEnd <> " < $1"
                                      <> ")"









type SubRowResource = (RES.ResourceId, Day, Maybe Day)
type SubRowBooking  = (BOK.BookingId, Day, PRJ.ProjectId, Int16)
type SubRowSchedule = (SCH.ScheduleId, Day, [Int16])
type SubRowTimeoff  = (TOF.TimeoffId, Day, Day)


type TableRow = (SubRowResource, SubRowSchedule, OptionalData)

data OptionalData = RowBookingData SubRowBooking 
                  | RowTimeoffData SubRowTimeoff
                  | RowNoData
                  deriving Show



readTableRow :: D.Row TableRow
readTableRow = (,,) <$> readSubRowResource
                    <*> readSubRowSchedule
                    <*> ( mkOptionalData <$> readSubRowBooking
                                         <*> readSubRowTimeoff )
                                        

 where mkOptionalData :: Maybe SubRowBooking -> Maybe SubRowTimeoff -> OptionalData
       mkOptionalData (Just b) _ = RowBookingData b
       mkOptionalData _ (Just t) = RowTimeoffData t
       mkOptionalData _ _        = RowNoData
 

       readSubRowResource :: D.Row SubRowResource
       readSubRowResource = (,,) <$> D.value ID.sqlDecoder
                                 <*> D.value D.date
                                 <*> D.nullableValue D.date


       readSubRowSchedule :: D.Row SubRowSchedule
       readSubRowSchedule = (,,) <$> D.value ID.sqlDecoder
                                 <*> D.value D.date
                                 <*> (D.value $ D.array $ D.arrayDimension replicateM $ D.arrayValue D.int2)


       readSubRowTimeoff :: D.Row (Maybe SubRowTimeoff)
       readSubRowTimeoff = f3 <$> t
         where t = (,,) <$> D.nullableValue ID.sqlDecoder
                        <*> D.nullableValue D.date
                        <*> D.nullableValue D.date


       readSubRowBooking :: D.Row (Maybe SubRowBooking)
       readSubRowBooking = f4 <$> t
         where t = (,,,) <$> D.nullableValue ID.sqlDecoder
                         <*> D.nullableValue D.date
                         <*> D.nullableValue ID.sqlDecoder
                         <*> D.nullableValue D.int2


       f3 (Just a, Just b, Just c) = Just $ (a, b, c)
       f3 _ = Nothing

       f4 (Just a, Just b, Just c, Just d) = Just $ (a, b, c, d)
       f4 _ = Nothing




getTableRows :: Query (Day, Day) [TableRow]
getTableRows  = mkQuery queryTxt
                        (contramap fst (E.value E.date) <> contramap snd (E.value E.date))
                        (D.rowsList readTableRow)
                        True




data ResourceData = ResourceData { resStart    :: Day
                                 , resEnd      :: Maybe Day
                                 , resSchedule :: SubRowSchedule
                                 , resBookings :: [SubRowBooking]
                                 , resTimeoffs :: [SubRowTimeoff]
                                 }
                                 deriving Show


type ResMap = [(RES.ResourceId, ResourceData)]


getResourcesData :: Query DateRange ResMap
getResourcesData = mkQuery queryTxt
                           dateRangeEncoder
                           (D.foldlRows rowToResMap [] readTableRow)
                           True


  where rowToResMap :: ResMap -> TableRow -> ResMap
        rowToResMap ((rid', resData') : rs) ((rid, _, _), _, rowData) | rid' == rid = (rid, appendResData resData' rowData) : rs
        rowToResMap acc ((rid, rStart, rEnd), rsch, rowData) = (rid, mkResData rowData rStart rEnd rsch) : acc


        mkResData :: OptionalData -> Day -> Maybe Day -> SubRowSchedule -> ResourceData
        mkResData rowData = case rowData of (RowBookingData x) -> mkData [x] []
                                            (RowTimeoffData x) -> mkData [] [x]
                                            _ -> mkData [] []
           where mkData bs ts st en sh = ResourceData st en sh bs ts


        appendResData :: ResourceData -> OptionalData -> ResourceData
        appendResData r@(ResourceData { resBookings = bs }) (RowBookingData x) = r { resBookings = x : bs }
        appendResData r@(ResourceData { resTimeoffs = ts }) (RowTimeoffData x) = r { resTimeoffs = x : ts }
        appendResData x _ = x



type SchedulesMap = [(SCH.ScheduleId, [(Day, PlanResCapacity)])]


getAllInRangeQ :: Query DateRange [PlanResRow]
getAllInRangeQ = proc dr@(DateRange d1 d2) -> do resMap <- getResourcesData -< dr
                                                 let sch = getSchedules resMap d1 d2
                                                     prr = toPlanResRow d1 d2 sch <$> resMap
                                                 returnA -< prr

  where getSchedules :: ResMap -> Day -> Day -> SchedulesMap
        getSchedules rs d1 d2 = foldl' f [] rs
          where f :: SchedulesMap -> (RES.ResourceId, ResourceData) -> SchedulesMap
                f sch (_, ResourceData { resSchedule = (sid, ds, ms)} ) = 
                  if any ((==sid) . fst) sch then sch
                                             else let daysMins = SCH.calculateFor' d1 d2 ds ms 
                                                  in (sid, (fmap . fmap) (mkCapacity sid) daysMins) : sch

                mkCapacity :: SCH.ScheduleId -> Int16 -> PlanResCapacity
                mkCapacity sid 0 = NoneDueSchedule sid
                mkCapacity _   x = Some x


        toPlanResRow :: Day -> Day -> SchedulesMap -> (RES.ResourceId, ResourceData) -> PlanResRow
        toPlanResRow d1 d2 sch (rid, ResourceData rStart rEnd (schId, _, _) rBookings rTimeoffs) = PlanResRow rid projRows caps
          where caps = case lookup schId sch of Nothing -> noSch
                                                Just x -> (checkTimeoffs . checkInactive) <$> x

                noSch = [d1 .. d2] `zip` (repeat ScheduleMissing)


                checkInactive :: (Day, PlanResCapacity) -> (Day, PlanResCapacity)
                checkInactive (d, _) | d < rStart || isJust (mfilter (< d) rEnd) = (d, ResourceInactive rStart rEnd)
                checkInactive x = x

                checkTimeoffs :: (Day, PlanResCapacity) -> (Day, PlanResCapacity)
                checkTimeoffs t@(d, Some _) = case found of Just x  -> (d, NoneDueTimeoff x)
                                                            Nothing -> t
                  where found = do (i, _, _) <- find (\(_, tst, tend) -> d >= tst && d <= tend) rTimeoffs
                                   return i
                checkTimeoffs x = x


                projRows = foldl' foldBookings [] rBookings
                  where foldBookings :: [PlanResProjectRow] -> SubRowBooking -> [PlanResProjectRow]
                        foldBookings (a@(PlanResProjectRow pid pbrows ) : acc) (bid, bday, bproj, bamt) | bproj == pid = (a { bookings = (PlanResBooking bid bday bamt) : pbrows }) : acc
                        foldBookings acc brow = (mkProjRow brow) : acc

                        mkProjRow (bid, bday, bproj, bamt) = PlanResProjectRow bproj $ [PlanResBooking bid bday bamt]




