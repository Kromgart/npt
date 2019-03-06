{-# LANGUAGE OverloadedStrings #-}

module NPT.Data.Timeoff where


import qualified NPT.Data.DataId as ID
import qualified NPT.Data.Resource as RES
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


import Data.Functor.Contravariant (contramap)
import Data.Int (Int16, Int64)
import Data.Monoid ((<>))
import Data.Time.Clock (UTCTime)
import Data.Time.Calendar (Day)

import Hasql.Query (Query)
import Hasql.Encoders ( Params
                      , int2
                      , int8
                      , date
                      , timestamptz
                      , value
                      , nullableValue
                      )

import qualified Hasql.Decoders as D



data TimeoffType = Vacation
                 | Sickness
                 | UnknownType
                 deriving Show


typeValue :: TimeoffType -> Int16
typeValue Vacation = 0
typeValue Sickness = 1
typeValue UnknownType = (-1)


intToType :: Int16 -> TimeoffType
intToType 0 = Vacation
intToType 1 = Sickness
intToType _ = UnknownType


data TimeoffStatus = Requested
                   | Approved
                   | UnknownStatus
                   deriving Show


statusValue :: TimeoffStatus -> Int16
statusValue Requested = 0
statusValue Approved  = 1
statusValue UnknownStatus = (-1)


intToStatus :: Int16 -> TimeoffStatus
intToStatus 0 = Requested
intToStatus 1 = Approved
intToStatus _ = UnknownStatus


data Timeoff = Timeoff { externalId  :: Maybe Int64
                       , resourceId  :: RES.ResourceId
                       , startDate   :: Day
                       , endDate     :: Day                       
                       , lastUpdated :: UTCTime
                       , timeoffType :: TimeoffType
                       , status      :: TimeoffStatus
                       }
                       deriving Show


type TimeoffId = ID.DataId Timeoff


mkTimeoff :: Maybe Int64
          -> RES.ResourceId
          -> Day
          -> Day
          -> UTCTime
          -> TimeoffType
          -> TimeoffStatus
          -> Either String Timeoff

mkTimeoff _extid _resid _start _end _upd _typ _stat
          | _start > _end = Left "Cannot create Timeoff: startDate must be less than endDate"
          | otherwise = Right $ Timeoff _extid _resid _start _end _upd _typ _stat



colPK    :: (ColumnName, Params TimeoffId, D.Row TimeoffId)

colExtId :: (ColumnName, Params Timeoff, D.Row (Maybe Int64))
colResId :: (ColumnName, Params Timeoff, D.Row RES.ResourceId)
colStart :: (ColumnName, Params Timeoff, D.Row Day)
colEnd   :: (ColumnName, Params Timeoff, D.Row Day)
colUpd   :: (ColumnName, Params Timeoff, D.Row UTCTime)
colTyp   :: (ColumnName, Params Timeoff, D.Row TimeoffType)
colStat  :: (ColumnName, Params Timeoff, D.Row TimeoffStatus)


colPK    = (ColumnName "timeoff_id", value ID.sqlEncoder, D.value ID.sqlDecoder)

colExtId = (ColumnName "external_id",  contramap externalId  $ nullableValue int8 , D.nullableValue D.int8)
colResId = (ColumnName "resource_id",  contramap resourceId  $ value ID.sqlEncoder, D.value ID.sqlDecoder)
colStart = (ColumnName "start_date",   contramap startDate   $ value date         , D.value D.date)
colEnd   = (ColumnName "end_date",     contramap endDate     $ value date         , D.value D.date)
colUpd   = (ColumnName "last_updated", contramap lastUpdated $ value timestamptz  , D.value D.timestamptz)
colTyp   = (ColumnName "timeoff_type", contramap (typeValue . timeoffType) $ value int2, intToType <$> D.value D.int2)
colStat  = (ColumnName "status",       contramap (statusValue . status)    $ value int2, intToStatus <$> D.value D.int2)


cols :: [(ColumnName, Params Timeoff)]
cols = [ tup3to2 colStart
       , tup3to2 colEnd
       , tup3to2 colUpd
       , tup3to2 colTyp
       , tup3to2 colStat
       ]



tblName :: TableName
tblName = TableName "timeoffs"


importQ :: Query Timeoff ()
importQ = mkQuery (mkInsertStmt tblName (fst <$> cols'')
                    <> mkConflictStmt (getColName colExtId) (fst <$> cols')
                    <> ";"
                  )
                  (foldMap snd cols'')
                  D.unit
                  True

  where cols' = tup3to2 colResId : cols
        cols'' = tup3to2 colExtId : cols'


readRow :: D.Row (TimeoffId, Timeoff)
readRow = (,) <$> getColDec colPK
              <*> ( Timeoff <$> getColDec colExtId
                            <*> getColDec colResId
                            <*> getColDec colStart
                            <*> getColDec colEnd
                            <*> getColDec colUpd
                            <*> getColDec colTyp
                            <*> getColDec colStat )




getAllInRangeQ :: Query DateRange [(TimeoffId, Timeoff)] 
getAllInRangeQ = mkQuery (mkSelectStmt tblName (getColName colPK : getColName colExtId : getColName colResId : fmap fst cols)
                          <> " WHERE NOT (" `addColName3` colStart <> " > $2 OR " `addColName3` colEnd <> " < $1);"
                         )
                         dateRangeEncoder
                         (D.rowsList readRow)
                         True





