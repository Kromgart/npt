{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module NPT.Data.WorkOrdersExecutiveTable where


import NPT.Data.Internal ( mkQuery
                         , DateRange
                         , dateRangeEncoder
                         )

import qualified NPT.Data.DataId as ID
import qualified NPT.Data.Group as GRP
import qualified NPT.Data.Project as PRJ
import qualified NPT.Data.WorkOrder as WO


import Data.Functor.Contravariant (contramap)
import Data.Int (Int32, Int64)
import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Data.Text (Text)


import GHC.Generics (Generic)

import Hasql.Query (Query)
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D


data WOExRow = WOExRow { projectId     :: PRJ.ProjectId
                       , bookedTotal   :: Int32
                       , bookedCurrent :: Int32
                       , loggedTotal   :: Int32
                       , workOrder     :: Maybe WOExWorkOrder
                       , groups        :: [WOExGroup]
                       }
                       deriving (Generic, Show)


data WOExWorkOrder = WOExWorkOrder { workOrderId :: WO.WorkOrderId
                                   , budget      :: Int32
                                   , extraBudget :: Int32
                                   , nonBillable :: Int32
                                   } deriving (Generic, Show)


data WOExGroup = WOExGroup { groupId :: GRP.GroupId
                           , booked  :: Int32
                           , logged  :: Int32
                           }
                           deriving (Generic, Show)
                           

data WOTblRow = WOTblRow { tblProjectId     :: PRJ.ProjectId
                         , tblTotalBooked   :: Int32
                         , tblCurrentBooked :: Int32
                         , tblTotalLogged   :: Int32
                         , tblWorkOrder     :: Maybe WOExWorkOrder
                         , tblGroup         :: Maybe WOExGroup
                         }


queryTxt :: Text
queryTxt = "SELECT project_id, book_total, book_current, log_total, workorder_id, budget, extra_budget, non_billable, group_id, book_minutes, log_minutes "
           <> "FROM npt_get_workorders_table_executive ($1, $2, $3);"


readTableRow :: D.Row WOTblRow
readTableRow = WOTblRow <$> D.value ID.sqlDecoder
                        <*> D.value D.int4
                        <*> D.value D.int4
                        <*> D.value D.int4
                        <*> ( mkWO <$> D.nullableValue ID.sqlDecoder
                                   <*> D.nullableValue D.int4
                                   <*> D.nullableValue D.int4 
                                   <*> D.nullableValue D.int4 )
                        <*> ( mkGrp <$> D.nullableValue ID.sqlDecoder
                                    <*> D.value D.int4
                                    <*> D.value D.int4 )

  where mkGrp :: Maybe GRP.GroupId -> Int32 -> Int32 -> Maybe WOExGroup
        mkGrp (Just i) b l = Just $ WOExGroup i b l
        mkGrp Nothing _ _ = Nothing
        
        mkWO :: Maybe WO.WorkOrderId -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe WOExWorkOrder
        mkWO i b exb nb = WOExWorkOrder <$> i <*> b <*> exb <*> nb 


getAllInRangeQ :: Query (DateRange, [GRP.GroupId]) [WOExRow]
getAllInRangeQ  = mkQuery queryTxt
                          (contramap fst dateRangeEncoder <> contramap snd (E.value ID.sqlEncoderList))
                          (D.foldlRows folder [] readTableRow)
                          True


folder :: [WOExRow] -> WOTblRow -> [WOExRow]
folder (x@(WOExRow { projectId = projId, groups = grps }) : xs) (WOTblRow tblPID _ _ _ tblWO tblGrp)
  | projId == tblPID = x { workOrder = tblWO, groups = newGrps } : xs
  where newGrps = case tblGrp of Nothing -> grps
                                 Just g -> g : grps

folder xs (WOTblRow pid tblBk tblBkCur tblLg tblWO tblGrp) = (WOExRow pid tblBk tblBkCur tblLg tblWO $ maybeToList tblGrp) : xs
        





