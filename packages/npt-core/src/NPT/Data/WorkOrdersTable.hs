{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module NPT.Data.WorkOrdersTable where


import NPT.Data.Internal ( mkQuery
                         , addColName3
                         , addTable
                         , runColumnName
                         , DateRange
                         , dateRangeEncoder
                         )

import qualified NPT.Data.Booking as BOK
import qualified NPT.Data.WorkOrder as WO


import Data.Int (Int32)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as TXT


import GHC.Generics (Generic)

import Hasql.Query (Query)
import qualified Hasql.Decoders as D


data WORow = WORow { workOrder :: (WO.WorkOrderId, WO.WorkOrder)
                   , bookedTotal   :: Int32
                   , bookedCurrent :: Int32
                   , logged        :: Int32
                   }
                   deriving (Generic, Show)


queryTxt :: Text
queryTxt = "SELECT w.workorder_id, w.project_id, w.budget, w.extra_budget, w.non_billable, w.start_date, w.end_date, w.is_approved," 
              <> " w.book_total, w.book_current, w.log_total "
              <> " FROM npt_get_workorders_table ($1, $2) w;"
               

readTableRow :: D.Row WORow
readTableRow = WORow <$> WO.readRow
                     <*> D.value D.int4
                     <*> D.value D.int4
                     <*> D.value D.int4
                                        

getAllInRangeQ :: Query DateRange [WORow]
getAllInRangeQ  = mkQuery queryTxt
                          dateRangeEncoder
                          (D.rowsList readTableRow)
                          True



