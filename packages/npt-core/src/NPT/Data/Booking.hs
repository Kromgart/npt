{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module NPT.Data.Booking where


import qualified NPT.Data.DataId as ID
import qualified NPT.Data.Resource as RES
import qualified NPT.Data.Project as PRJ
import NPT.Data.Internal ( mkQuery
                         , mkDeleteQuery
                         , mkSelectStmt
                         , mkInsertStmt
                         , mkUpdateStmt
                         , ColumnName (ColumnName)
                         , TableName (TableName)
                         , addColName3
                         , getColName
                         , getColDec
                         , getColEnc
                         , tup3to2
                         , DateRange
                         , dateRangeEncoder
                         )

import Data.Functor.Contravariant (contramap)
import Data.Int ( Int16
                , Int64
                )
import Data.Monoid ((<>))
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)

import GHC.Generics (Generic)

import Hasql.Query (Query)
import qualified Hasql.Decoders as D
import Hasql.Encoders ( Params
                      , value
                      , int2
                      , date
                      , timestamptz 
                      )





data Booking = Booking { resourceId  :: RES.ResourceId
                       , bookDate    :: Day
                       , projectId   :: PRJ.ProjectId
                       , amount      :: Int16
                       }
                       deriving (Show, Generic)


type BookingId = ID.DataId Booking


colPK     :: (ColumnName, Params BookingId, D.Row BookingId)

colResId  :: (ColumnName, Params Booking, D.Row RES.ResourceId)
colDate   :: (ColumnName, Params Booking, D.Row Day)
colProjId :: (ColumnName, Params Booking, D.Row PRJ.ProjectId)
colAmount :: (ColumnName, Params Booking, D.Row Int16)
colUpd    :: (ColumnName, Params UTCTime, D.Row UTCTime)


colPK     = (ColumnName "booking_id", value ID.sqlEncoder, D.value ID.sqlDecoder)

colResId  = (ColumnName "resource_id", contramap resourceId   $ value ID.sqlEncoder, D.value ID.sqlDecoder)
colDate   = (ColumnName "book_date",   contramap bookDate     $ value date         , D.value D.date)
colProjId = (ColumnName "project_id",  contramap projectId    $ value ID.sqlEncoder, D.value ID.sqlDecoder)
colAmount = (ColumnName "amount",      contramap amount       $ value int2         , D.value D.int2)

colUpd    = (ColumnName "last_updated", value timestamptz, D.value D.timestamptz)


paramCols :: [(ColumnName, Params Booking)]
paramCols = [ tup3to2 colResId
            , tup3to2 colDate
            , tup3to2 colProjId
            , tup3to2 colAmount
            ]


tblName :: TableName
tblName = TableName "bookings"


insertQ :: Query Booking BookingId
insertQ = mkQuery (mkInsertStmt tblName (fst <$> paramCols) <> " RETURNING " `addColName3` colPK <> ";") 
                  (foldMap snd $ paramCols) 
                  (D.singleRow $ getColDec colPK)
                  True


updateQ :: Query (BookingId, Booking) Int64 
updateQ = mkQuery (mkUpdateStmt tblName (getColName colPK) (fst <$> paramCols) <> ";") 
                  (contramap fst (getColEnc colPK) <> (foldMap (contramap snd . snd) paramCols) )
                  D.rowsAffected
                  True


deleteQ :: Query BookingId ()
deleteQ = mkDeleteQuery tblName $ getColName colPK


readRow :: D.Row (BookingId, Booking)
readRow = (,) <$> getColDec colPK
              <*> (Booking <$> getColDec colResId
                           <*> getColDec colDate
                           <*> getColDec colProjId
                           <*> getColDec colAmount )



getAllInRangeQ :: Query DateRange [(BookingId, Booking)]
getAllInRangeQ = mkQuery (mkSelectStmt tblName (getColName colPK : (fst <$> paramCols))
                         <> " WHERE " `addColName3` colDate <> " >= $1 AND "
                                      `addColName3` colDate <> " <= $2 ;")
                         dateRangeEncoder
                         (D.rowsList readRow)
                         True


