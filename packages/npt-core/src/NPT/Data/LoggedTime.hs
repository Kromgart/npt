{-# LANGUAGE OverloadedStrings #-}

module NPT.Data.LoggedTime where


import qualified NPT.Data.DataId as ID
import qualified NPT.Data.Resource as RES
import qualified NPT.Data.Project as PRJ
import NPT.Data.Internal ( mkQuery
                         , mkSelectStmt
                         , mkInsertStmt
                         , mkConflictStmt
                         , ColumnName (ColumnName)
                         , TableName (TableName)
                         , addColName3
                         , getColName
                         , getColDec
                         , tup3to2
                         , DateRange
                         , dateRangeEncoder
                         )

import Data.Time.Clock (UTCTime)
import Data.Functor.Contravariant (contramap)
import Data.Int ( Int16
                , Int64)
import Data.Monoid ((<>))
import Data.Time.Calendar (Day)

import Hasql.Query (Query)
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D



data LoggedTime = LoggedTime { externalId :: Maybe Int64
                             , resourceId :: RES.ResourceId
                             , projectId  :: PRJ.ProjectId
                             , logDate    :: Day                       
                             , logMinutes :: Int16
                             }
                             deriving Show


type LoggedTimeId = ID.DataId LoggedTime

type LoggedTimeTstmp = UTCTime 

mkLoggedTime :: Maybe Int64
             -> RES.ResourceId
             -> PRJ.ProjectId
             -> Day
             -> Int16
             -> Either String LoggedTime

mkLoggedTime _extid _resid _projid _date _mins 
          | _mins <= 0 = Left "Cannot create LoggedTime: must be positive duration"
          | otherwise = Right $ LoggedTime _extid _resid _projid _date _mins



colPK     :: (ColumnName, E.Params LoggedTimeId, D.Row LoggedTimeId)

colExtId  :: (ColumnName, E.Params LoggedTime, D.Row (Maybe Int64))
colResId  :: (ColumnName, E.Params LoggedTime, D.Row RES.ResourceId)
colProjId :: (ColumnName, E.Params LoggedTime, D.Row PRJ.ProjectId)
colDate   :: (ColumnName, E.Params LoggedTime, D.Row Day)
colMins   :: (ColumnName, E.Params LoggedTime, D.Row Int16)

colLstUpd :: (ColumnName, E.Params (), D.Row UTCTime)


colPK     = (ColumnName "log_id"     , E.value ID.sqlEncoder, D.value ID.sqlDecoder)

colExtId  = (ColumnName "external_id", contramap externalId $ E.nullableValue E.int8, D.nullableValue D.int8)
colResId  = (ColumnName "resource_id", contramap resourceId $ E.value ID.sqlEncoder,  D.value ID.sqlDecoder)
colProjId = (ColumnName "project_id",  contramap projectId  $ E.value ID.sqlEncoder,  D.value ID.sqlDecoder)
colDate   = (ColumnName "log_date"   , contramap logDate    $ E.value E.date,         D.value D.date)
colMins   = (ColumnName "log_minutes", contramap logMinutes $ E.value E.int2,         D.value D.int2)

colLstUpd = (ColumnName "last_updated", E.unit, D.value D.timestamptz)

cols :: [(ColumnName, E.Params LoggedTime)]
cols = [ tup3to2 colExtId 
       , tup3to2 colResId 
       , tup3to2 colProjId
       , tup3to2 colDate
       , tup3to2 colMins
       ]


tblName :: TableName
tblName = TableName "loggedtime"


readRow :: D.Row (LoggedTimeId, LoggedTime)
readRow = (,) <$> getColDec colPK
              <*> ( LoggedTime <$> getColDec colExtId
                               <*> getColDec colResId
                               <*> getColDec colProjId
                               <*> getColDec colDate
                               <*> getColDec colMins )



getAllInRangeQ :: Query DateRange [(LoggedTimeId, LoggedTime)] 
getAllInRangeQ = mkQuery (mkSelectStmt tblName (getColName colPK : fmap fst cols)
                          <> " WHERE (" `addColName3` colDate <> " >= $1 AND " `addColName3` colDate <> " <= $2);"
                         )
                         dateRangeEncoder
                         (D.rowsList readRow)
                         True

importLoggedTimeQ :: Query LoggedTime LoggedTimeId
importLoggedTimeQ = mkQuery (mkInsertStmt tblName (fst <$> cols) 
                    <> mkConflictStmt (getColName colExtId) (fst <$> cols)
                    <> "," `addColName3` colLstUpd <> " = now()"
                    <> " RETURNING " `addColName3` colPK <> ", " `addColName3` colExtId
                    <> ";")  
                    (foldMap snd cols)
                    (D.singleRow $ getColDec colPK)
                    True

getLastImportTsmp :: Query () UTCTime
getLastImportTsmp = mkQuery (mkSelectStmt tblName (getColName colLstUpd : [])
                            <> " ORDER BY " `addColName3` colLstUpd <> " DESC LIMIT 1")
                            E.unit
                            (D.singleRow $ getColDec colLstUpd)
                            True
