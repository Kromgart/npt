{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NPT.Data ( module NPT.Data
                , TransQuery (TransQuery)
                , mkBatchA
                , DateRange
                , mkDateRange
                )

where


import NPT.Data.Internal ( TransQuery (TransQuery)
                         , mkBatchA
                         , DateRange
                         , mkDateRange
                         )
import qualified NPT.Data.DataId as ID
import qualified NPT.Data.Resource as RES
import qualified NPT.Data.GroupType as GRPT
import qualified NPT.Data.Group as GRP
import qualified NPT.Data.ProjectType as PT
import qualified NPT.Data.Project  as PRJ
import qualified NPT.Data.Booking  as BOK
import qualified NPT.Data.Schedule as SCH
import qualified NPT.Data.Timeoff  as TOF
import qualified NPT.Data.WorkOrder as WOR
import qualified NPT.Data.LoggedTime as TIM
import qualified NPT.Data.ResourcePlanningTable as RPT
import qualified NPT.Data.WorkOrdersTable as WOT
import qualified NPT.Data.WorkOrdersExecutiveTable as WOXT
import qualified NPT.Data.User as USR


import Control.Monad.Trans.Except ( ExceptT (ExceptT)
                                  , withExceptT
                                  , throwE)

import Data.Int (Int64)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Word (Word16)


import Hasql.Connection ( Connection
                        , ConnectionError
                        , acquire
                        , settings
                        , release )
import Hasql.Session ( Session
                     , Error
                     , query
                     , run )
import Hasql.Query (Query)

import qualified Hasql.Transaction as HQT
import qualified Hasql.Transaction.Sessions as HQTS



data DBSettings = DBSettings { dbhost     :: ByteString
                             , dbport     :: Word16
                             , dblogin    :: ByteString
                             , dbpassword :: ByteString
                             , dbname     :: ByteString
                             }


type ConnectionString = ByteString


mkConnectionString :: DBSettings -> ConnectionString
mkConnectionString (DBSettings { dbhost = hst, dbport = prt, dblogin = lgn, dbpassword = pwd, dbname = dbnm}) = settings hst prt lgn pwd dbnm


getConn :: ConnectionString -> ExceptT ConnectionError IO Connection
getConn cs = ExceptT $ acquire cs


data DataError = ConnError ConnectionError
               | ExecError Error
               | AuthenticationError String
               | AuthorizationError String
               | OtherError String
               deriving Show


type DBResult a = ExceptT DataError IO a


type KeyedObject a = (ID.DataId a, a)


runSingleSession :: forall a. ConnectionString -> Session a -> DBResult a
runSingleSession conn sess = do c <- withExceptT ConnError $ getConn conn
                                withExceptT ExecError $ f c 
  where f :: Connection -> ExceptT Error IO a
        f c = ExceptT $ run sess c <* release c

 
runSingleQ :: ConnectionString -> Query a b -> a -> DBResult b
runSingleQ s q x = runSingleSession s $ query x q


runTransQ :: ConnectionString -> TransQuery a b -> a -> DBResult b
runTransQ s (TransQuery q) x = runSingleSession s $ HQTS.transaction HQT.ReadCommitted HQT.Write $ HQT.query x q


checkRowsAffected :: Int64 -> Int64 -> DBResult ()
checkRowsAffected target affected | target == affected = return ()
                                  | otherwise = throwE $ OtherError $ "Operation updated " <> show affected <> "rows instead of " <> show target 


-------- Resources ---------

getResources :: ConnectionString -> DBResult [KeyedObject RES.Resource]
getResources s = runSingleQ s RES.getAllQ ()


insertOneResource :: ConnectionString -> RES.Resource -> DBResult RES.ResourceId
insertOneResource s = runTransQ s RES.insertTQ


-------- Projects ---------


getProjects :: ConnectionString -> DBResult [KeyedObject PRJ.Project]
getProjects s = runSingleQ s PRJ.getAllQ ()


insertProject :: ConnectionString -> PRJ.Project -> DBResult PRJ.ProjectId
insertProject s = runSingleQ s PRJ.insertQ


-------- Workorders ---------


getWorkOrders :: ConnectionString -> DBResult [KeyedObject WOR.WorkOrder]
getWorkOrders s = runSingleQ s WOR.getAllQ ()


insertWorkOrder :: ConnectionString -> WOR.WorkOrder -> DBResult WOR.WorkOrderId
insertWorkOrder s = runSingleQ s WOR.insertQ


updateWorkOrder :: ConnectionString -> WOR.WorkOrderId -> WOR.WorkOrder -> DBResult ()
updateWorkOrder s i w = runSingleQ s WOR.updateQ (i, w) >>= checkRowsAffected 1


deleteWorkOrder :: ConnectionString -> WOR.WorkOrderId -> DBResult ()
deleteWorkOrder s = runSingleQ s WOR.deleteQ


-------- Schedules ---------


getSchedules :: ConnectionString -> DBResult [KeyedObject SCH.Schedule]
getSchedules s = runSingleQ s SCH.getAllQ ()


-------- Groups/GroupTypes --------


getGroupTypes :: ConnectionString -> DBResult [KeyedObject GRPT.GroupType]
getGroupTypes s = runSingleQ s GRPT.getAllQ ()


getGroups :: ConnectionString -> DBResult [KeyedObject GRP.Group]
getGroups s = runSingleQ s GRP.getAllQ ()


getGroupsByType :: ConnectionString -> GRPT.GroupTypeId -> DBResult [KeyedObject GRP.Group]
getGroupsByType s i = runSingleQ s GRP.getAllByTypeQ i


-------- Tags/TagTypes --------


getProjectTypes :: ConnectionString -> DBResult [KeyedObject PT.ProjectType]
getProjectTypes s = runSingleQ s PT.getAllQ ()


-------- Bookings --------

getAllBookingsInRange :: ConnectionString -> DateRange -> DBResult [KeyedObject BOK.Booking]
getAllBookingsInRange s dr = runSingleQ s BOK.getAllInRangeQ dr


insertBooking :: ConnectionString -> BOK.Booking -> DBResult BOK.BookingId
insertBooking s b = runSingleQ s BOK.insertQ b


updateBooking :: ConnectionString -> BOK.BookingId -> BOK.Booking -> DBResult ()
updateBooking s i b = runSingleQ s BOK.updateQ (i, b) >>= checkRowsAffected 1


deleteBooking :: ConnectionString -> BOK.BookingId -> DBResult ()
deleteBooking s i = runSingleQ s BOK.deleteQ i


-------- Timeoffs --------

getAllTimeoffsInRange :: ConnectionString -> DateRange -> DBResult [KeyedObject TOF.Timeoff]
getAllTimeoffsInRange s dr = runSingleQ s TOF.getAllInRangeQ dr



-------- Logged Time --------

getAllLoggedTimeInRange :: ConnectionString -> DateRange -> DBResult [KeyedObject TIM.LoggedTime]
getAllLoggedTimeInRange s dr = runSingleQ s TIM.getAllInRangeQ dr



-------- Resource Planning Table ---------

getResourcePlanningTable :: ConnectionString -> DateRange -> DBResult [RPT.PlanResRow]
getResourcePlanningTable s dr = runSingleQ s RPT.getAllInRangeQ dr


-------- WorkOrders Table ---------

getWorkOrdersTable :: ConnectionString -> DateRange -> DBResult [WOT.WORow]
getWorkOrdersTable s dr = runSingleQ s WOT.getAllInRangeQ dr


-------- WorkOrders Executive Table ---------

getWorkOrdersExecutiveTable :: ConnectionString -> DateRange -> [GRP.GroupId] -> DBResult [WOXT.WOExRow]
getWorkOrdersExecutiveTable s dr gs = runSingleQ s WOXT.getAllInRangeQ (dr, gs)


-------- Users ------------

loginUser :: ConnectionString -> USR.Username -> USR.Password -> DBResult (KeyedObject USR.User)
loginUser s u p = do res <- runSingleQ s USR.loginUserQ (u, p)
                     case res of Nothing -> throwE $ AuthenticationError "Invalid credentials"
                                 Just x -> return x
                                 



