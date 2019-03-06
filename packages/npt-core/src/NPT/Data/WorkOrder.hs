{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module NPT.Data.WorkOrder where


import qualified NPT.Data.DataId as ID
import qualified NPT.Data.Project as PRJ
import NPT.Data.Internal ( mkQuery
                         , mkDeleteQuery
                         , mkInsertStmt
                         , mkUpdateStmt
                         , mkSelectStmt
                         , ColumnName (ColumnName)
                         , TableName (TableName)
                         , addColName3
                         , getColDec
                         , getColEnc
                         , getColName
                         , tup3to2
                         )


import Data.Functor.Contravariant (contramap)
import Data.Int (Int32, Int64)
import Data.Monoid ((<>))
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)

import Hasql.Query (Query)
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E


{- Instances for Elm and Aeson are orphaned in respective executables -}

data WorkOrder = WorkOrder { projectId   :: PRJ.ProjectId
                           , budget      :: Int32
                           , extraBudget :: Int32
                           , nonBillable :: Int32
                           , startDate   :: Day 
                           , endDate     :: Day 
                           , isApproved  :: Bool
                           }
                           deriving (Show, Generic)


type WorkOrderId = ID.DataId WorkOrder


colPK :: (ColumnName, E.Params WorkOrderId, D.Row WorkOrderId)
colPK = (ColumnName "workorder_id",  E.value ID.sqlEncoder, D.value ID.sqlDecoder)

colProjId      :: (ColumnName, E.Params WorkOrder, D.Row PRJ.ProjectId)
colBudget      :: (ColumnName, E.Params WorkOrder, D.Row Int32)
colExtraBudget :: (ColumnName, E.Params WorkOrder, D.Row Int32)
colNonBillable :: (ColumnName, E.Params WorkOrder, D.Row Int32)
colStartDate   :: (ColumnName, E.Params WorkOrder, D.Row Day)
colEndDate     :: (ColumnName, E.Params WorkOrder, D.Row Day)
colApproved    :: (ColumnName, E.Params WorkOrder, D.Row Bool)

colProjId      = (ColumnName "project_id",   contramap projectId   $ E.value ID.sqlEncoder, D.value ID.sqlDecoder)
colBudget      = (ColumnName "budget",       contramap budget      $ E.value E.int4, D.value D.int4)
colExtraBudget = (ColumnName "extra_budget", contramap extraBudget $ E.value E.int4, D.value D.int4)
colNonBillable = (ColumnName "non_billable", contramap nonBillable $ E.value E.int4, D.value D.int4)
colStartDate   = (ColumnName "start_date",   contramap startDate   $ E.value E.date, D.value D.date)
colEndDate     = (ColumnName "end_date",     contramap endDate     $ E.value E.date, D.value D.date)
colApproved    = (ColumnName "is_approved",  contramap isApproved  $ E.value E.bool, D.value D.bool)


cols :: [(ColumnName, E.Params WorkOrder)]
cols = [ tup3to2 colProjId
       , tup3to2 colBudget
       , tup3to2 colExtraBudget
       , tup3to2 colNonBillable
       , tup3to2 colStartDate
       , tup3to2 colEndDate
       , tup3to2 colApproved
       ]



tblName :: TableName
tblName = TableName "workorders"


insertQ :: Query WorkOrder WorkOrderId
insertQ = mkQuery (mkInsertStmt tblName (fst <$> cols) <> " RETURNING " `addColName3` colPK <> ";") 
                  (foldMap snd $ cols) 
                  (D.singleRow $ getColDec colPK)
                  True


updateQ :: Query (WorkOrderId, WorkOrder) Int64 
updateQ = mkQuery (mkUpdateStmt tblName (getColName colPK) (fst <$> cols) <> ";") 
                  (contramap fst (getColEnc colPK) <> (foldMap (contramap snd . snd) cols) )
                  D.rowsAffected
                  True


deleteQ :: Query WorkOrderId ()
deleteQ = mkDeleteQuery tblName $ getColName colPK


readRow :: D.Row (WorkOrderId, WorkOrder)
readRow = (,) <$> getColDec colPK
              <*> ( WorkOrder <$> getColDec colProjId
                              <*> getColDec colBudget
                              <*> getColDec colExtraBudget
                              <*> getColDec colNonBillable
                              <*> getColDec colStartDate
                              <*> getColDec colEndDate
                              <*> getColDec colApproved )


readColNames :: [ColumnName]
readColNames = getColName colPK : fmap fst cols


getAllQ :: Query () [(WorkOrderId, WorkOrder)]
getAllQ = mkQuery (mkSelectStmt tblName readColNames <> ";")
                  E.unit
                  (D.rowsList readRow)
                  True




