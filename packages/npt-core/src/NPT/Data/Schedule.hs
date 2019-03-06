{-# LANGUAGE OverloadedStrings #-}

module NPT.Data.Schedule where


import qualified NPT.Data.DataId as ID
import NPT.Data.Internal ( mkQuery
                         , mkSelectStmt
                         , ColumnName (ColumnName)
                         , TableName (TableName)
                         , tup3to2
                         , getColDec
                         , getColName
                         )


import Control.Monad (replicateM)
import Data.Functor.Contravariant (contramap)
import Data.Int (Int16)
import Data.List (foldl')
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Calendar (Day, diffDays)


import Hasql.Query (Query)
import qualified Hasql.Decoders as D
import Hasql.Encoders ( Params
                      , value
                      , array
                      , arrayDimension
                      , arrayValue
                      , int2
                      , text
                      , date
                      , unit )



data Schedule = Schedule { displayName :: Text
                         , startDate   :: Day
                         , minutes     :: [Int16] }
                         deriving Show


type ScheduleId = ID.DataId Schedule


calculateFor :: Day -> Day -> Schedule -> [(Day, Int16)]
calculateFor d1 d2 (Schedule _ st mins) = calculateFor' d1 d2 st mins
 

calculateFor' :: Day -> Day -> Day -> [Int16] -> [(Day, Int16)]
calculateFor' d1' d2' shStart shCycle = zip [d1 .. d2] tmpl'
  where (d1, d2) = if d1' <= d2' then (d1', d2')
                                 else (d2', d1')
          
        delta = fromIntegral $ diffDays d1 shStart
        h = delta `rem` (length shCycle)
        tmpl' = (drop h shCycle) ++ (cycle shCycle)


colPK    :: (ColumnName, Params ScheduleId, D.Row ScheduleId)

colName  :: (ColumnName, Params Schedule, D.Row Text)
colStart :: (ColumnName, Params Schedule, D.Row Day)
colCycle :: (ColumnName, Params Schedule, D.Row [Int16])


colPK    = (ColumnName "schedule_id",  value ID.sqlEncoder, D.value ID.sqlDecoder)

colName  = (ColumnName "display_name", contramap displayName $ value text, D.value D.text)
colStart = (ColumnName "start_date",   contramap startDate   $ value date, D.value D.date)
colCycle = (ColumnName "cycle_def",    contramap minutes $ value $ array $ arrayDimension foldl' $ arrayValue int2, 
                                       (D.value $ D.array $ D.arrayDimension replicateM $ D.arrayValue D.int2))


paramCols :: [(ColumnName, Params Schedule)]
paramCols = [ tup3to2 colName
            , tup3to2 colStart
            , tup3to2 colCycle
            ]


tblName :: TableName
tblName = TableName "schedules"


readRow :: D.Row (ScheduleId, Schedule)
readRow = (,) <$> getColDec colPK
              <*> ( Schedule <$> getColDec colName
                             <*> getColDec colStart
                             <*> getColDec colCycle )


getAllQ :: Query () [(ScheduleId, Schedule)]
getAllQ = mkQuery (mkSelectStmt tblName (getColName colPK : fmap fst paramCols) <> ";")
                  unit
                  (D.rowsList readRow)
                  True









