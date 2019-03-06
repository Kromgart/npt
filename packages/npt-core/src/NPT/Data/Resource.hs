{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module NPT.Data.Resource where


import qualified NPT.Data.DataId as ID
import qualified NPT.Data.Schedule as SCH
import qualified NPT.Data.Group as GRP
import NPT.Data.Internal ( mkQuery
                         , mkBatchA
                         , mkSelectStmt
                         , mkDeleteStmt
                         , mkInsertStmt
                         , mkConflictStmt
                         , tup3to2
                         , getColName
                         , getColDec
                         , ColumnName (ColumnName)
                         , TableName (TableName)
                         , addColName3
                         , addTable
                         , TransQuery (TransQuery)
                         )

import Control.Arrow (returnA)
import Control.Monad (replicateM)
import Data.Functor.Contravariant (contramap)
import Data.Int (Int64)
import Data.Maybe (fromJust, catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as TXT
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)

import Hasql.Query (Query)
import qualified Hasql.Decoders as D
import Hasql.Encoders ( Params
                      , value
                      , nullableValue
                      , int8
                      , text
                      , date
                      , unit )




{- Instances for Elm and Aeson are orphaned in respective executables -}

data Resource = Resource { externalId :: Maybe Int64
                         , supervisor :: Maybe ResourceId
                         , name       :: Text
                         , schedule   :: SCH.ScheduleId
                         , startDate  :: Day
                         , endDate    :: Maybe Day
                         , title      :: Text
                         , email      :: Maybe Text
                         , phone      :: Maybe Text 
                         , groups     :: [GRP.GroupId]
                         }
                         deriving (Show, Generic)


type ResourceId = ID.DataId Resource


mkResource :: Maybe Int64 
           -> Maybe ResourceId 
           -> Text 
           -> SCH.ScheduleId
           -> Day 
           -> Maybe Day 
           -> Text
           -> Maybe Text
           -> Maybe Text
           -> [GRP.GroupId]
           -> Either String Resource


mkResource _extId _svId _nm _sch _sdate _edate _title _email _phone _grps
           | TXT.null _nm    = Left "Name cannot be empty"
           | any ((>=) _sdate) _edate  = Left "StartDate must be less than EndDate"
           | otherwise       = Right $ Resource _extId _svId _nm _sch _sdate _edate _title _email _phone _grps



colPK    :: (ColumnName, Params ResourceId, D.Row ResourceId)

colExtId :: (ColumnName, Params Resource, D.Row (Maybe Int64))
colSupId :: (ColumnName, Params Resource, D.Row (Maybe ResourceId))
colName  :: (ColumnName, Params Resource, D.Row Text)
colStart :: (ColumnName, Params Resource, D.Row Day)
colEnd   :: (ColumnName, Params Resource, D.Row (Maybe Day))
colSchId :: (ColumnName, Params Resource, D.Row SCH.ScheduleId)
colTitle :: (ColumnName, Params Resource, D.Row Text)
colEmail :: (ColumnName, Params Resource, D.Row (Maybe Text))
colPhone :: (ColumnName, Params Resource, D.Row (Maybe Text))


colPK    = (ColumnName "resource_id",   value ID.sqlEncoder, D.value ID.sqlDecoder)

colExtId = (ColumnName "external_id",   contramap externalId $ nullableValue int8, D.nullableValue D.int8)
colSupId = (ColumnName "supervisor_id", contramap supervisor $ nullableValue ID.sqlEncoder, D.nullableValue ID.sqlDecoder)
colName  = (ColumnName "display_name",  contramap name       $ value         text, D.value         D.text)
colStart = (ColumnName "start_date",    contramap startDate  $ value         date, D.value         D.date)
colEnd   = (ColumnName "end_date",      contramap endDate    $ nullableValue date, D.nullableValue D.date)
colSchId = (ColumnName "schedule_id",   contramap schedule   $ value ID.sqlEncoder, D.value ID.sqlDecoder)
colTitle = (ColumnName "title",         contramap title      $ value         text, D.value         D.text)
colEmail = (ColumnName "email",         contramap email      $ nullableValue text, D.nullableValue D.text)
colPhone = (ColumnName "phone",         contramap phone      $ nullableValue text, D.nullableValue D.text)

paramCols :: [(ColumnName, Params Resource)]
paramCols = [ tup3to2 colName
            , tup3to2 colSchId
            , tup3to2 colStart
            , tup3to2 colEnd
            , tup3to2 colTitle
            , tup3to2 colEmail
            , tup3to2 colPhone
            ]




tblName :: TableName
tblName = TableName "resources"


rgTblName :: TableName
rgTblName = TableName "resources_groups"

rgColGroupId :: (ColumnName, Params (ResourceId, GRP.GroupId), D.Row GRP.GroupId)
rgColResId   :: (ColumnName, Params (ResourceId, GRP.GroupId), D.Row ResourceId)
rgColGroupId = (ColumnName "group_id",    contramap snd $ value ID.sqlEncoder, D.value ID.sqlDecoder)
rgColResId   = (ColumnName "resource_id", contramap fst $ value ID.sqlEncoder, D.value ID.sqlDecoder)


insertTQ :: TransQuery Resource ResourceId
insertTQ = proc r -> do rid <- TransQuery insertQ -< r
                        setResourceGroupsTQ -< (rid, groups r)
                        returnA -< rid

  where insertQ :: Query Resource ResourceId
        insertQ = mkQuery (mkInsertStmt tblName (fst <$> cs) <> " RETURNING " `addColName3` colPK <> ";") 
                          (foldMap snd cs) 
                          (D.singleRow $ getColDec colPK)
                          True
          where cs = (tup3to2 colExtId) : (tup3to2 colSupId) : paramCols


setResourceGroupsTQ :: TransQuery (ResourceId, [GRP.GroupId]) ()
setResourceGroupsTQ = TransQuery $ proc (rid, grps) -> do clearGroupsQ -< rid
                                                          mkBatchA insertGroupQ -< zip (repeat rid) grps
                                                          returnA -< ()

  where clearGroupsQ :: Query ResourceId ()
        clearGroupsQ = mkQuery (mkDeleteStmt rgTblName $ getColName rgColResId)
                               (value ID.sqlEncoder)
                               D.unit
                               True

        cs = [tup3to2 rgColGroupId, tup3to2 rgColResId]
        
        insertGroupQ :: Query (ResourceId, GRP.GroupId) ()
        insertGroupQ = mkQuery (mkInsertStmt rgTblName (fst <$> cs)
                                  <> ";"
                               )
                               (foldMap snd cs)
                               D.unit
                               True



importTQ :: TransQuery Resource (ResourceId, Int64)
importTQ = proc r -> do (rid, rext) <- TransQuery importQ -< r
                        setResourceGroupsTQ -< (rid, groups r)
                        returnA -< (rid, rext)

  where cols' = (tup3to2 colExtId) : paramCols

        importQ :: Query Resource (ResourceId, Int64)
        importQ = mkQuery (mkInsertStmt tblName (fst <$> cols') 
                             <> mkConflictStmt (getColName colExtId) (fst <$> paramCols)
                             <> " RETURNING " `addColName3` colPK <> ", " `addColName3` colExtId
                             <> ";")  
                          (foldMap snd cols')
                          (D.singleRow $ (,) <$> getColDec colPK 
                                             <*> (fromJust <$> getColDec colExtId))
                          True

        


resetEmailsQ :: Query () ()
resetEmailsQ = mkQuery ("UPDATE " `addTable` tblName <> " SET " `addColName3` colEmail <> " = NULL;")
                       unit 
                       D.unit 
                       False


setSupervisorQ :: Query (ResourceId, Maybe ResourceId) ()
setSupervisorQ = mkQuery ("UPDATE " `addTable` tblName <> " SET " `addColName3` colSupId <> " = $2 WHERE " `addColName3` colPK <> " = $1")
                         (contramap fst (value ID.sqlEncoder) <> contramap snd (nullableValue ID.sqlEncoder))
                         D.unit
                         True


readRow :: D.Row (ResourceId, Resource)
readRow = (,) <$> getColDec colPK
              <*> (Resource <$> getColDec colExtId
                            <*> getColDec colSupId
                            <*> getColDec colName
                            <*> getColDec colSchId
                            <*> getColDec colStart
                            <*> getColDec colEnd
                            <*> getColDec colTitle
                            <*> getColDec colEmail
                            <*> getColDec colPhone
                            <*> (D.value $ D.array $ fmap catMaybes $ D.arrayDimension replicateM $ D.arrayNullableValue ID.sqlDecoder)
                  )


getAllQ :: Query () [(ResourceId, Resource)]
getAllQ = mkQuery selectTxt unit (D.rowsList readRow) True
  where selectTxt = "SELECT r." `addColName3` colPK 
                      <> ", r." `addColName3` colExtId
                      <> ", r." `addColName3` colSupId
                      <> ", r." `addColName3` colName
                      <> ", r." `addColName3` colSchId
                      <> ", r." `addColName3` colStart
                      <> ", r." `addColName3` colEnd
                      <> ", r." `addColName3` colTitle
                      <> ", r." `addColName3` colEmail
                      <> ", r." `addColName3` colPhone
                      <> ", array_agg(rg." `addColName3` rgColGroupId <> ") as groups"
                      <> " FROM " `addTable` tblName <> " r"
                      <> " LEFT JOIN " `addTable` rgTblName <> " rg ON r." `addColName3` colPK <> " = rg." `addColName3` rgColResId
                      <> " GROUP BY r." `addColName3` colPK <> ";"

             


getResourceEmailsQ :: Query () [(Text, ResourceId)]
getResourceEmailsQ = mkQuery (mkSelectStmt tblName (getColName colEmail : [getColName colPK]) 
                               <> " WHERE " `addColName3` colEmail <> " IS NOT NULL;")
                             unit
                             (D.rowsList ((,) <$> D.value D.text <*> getColDec colPK))
                             True
