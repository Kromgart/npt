{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module NPT.Data.Project where


import qualified NPT.Data.DataId      as ID
import qualified NPT.Data.Resource    as RES
import qualified NPT.Data.ProjectType as PT
import NPT.Data.Internal ( mkQuery
                         , mkInsertStmt
                         , mkSelectStmt
                         , mkConflictStmt
                         , tup3to2
                         , getColName
                         , getColDec
                         , ColumnName (ColumnName)
                         , TableName (TableName)
                         , addColName3
                         )


import Data.Maybe (fromJust)
import Data.Functor.Contravariant (contramap)
import Data.Int (Int64)
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Generics (Generic)

import Hasql.Query (Query)
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E


{- Instances for Elm and Aeson are orphaned in respective executables -}

data Project = Project { externalId  :: Maybe Int64
                       , fullName    :: Text 
                       , displayName :: Text
                       , color       :: Text
                       , projectMgr  :: Maybe RES.ResourceId
                       , accountMgr  :: Maybe RES.ResourceId
                       , projectType :: PT.ProjectTypeId
                       }
                       deriving (Show, Generic)


type ProjectId = ID.DataId Project


tblName :: TableName
tblName = TableName "projects"


colPK :: (ColumnName, E.Params ProjectId, D.Row ProjectId)
colPK = (ColumnName "project_id", E.value ID.sqlEncoder, D.value ID.sqlDecoder)

colExtId       :: (ColumnName, E.Params Project, D.Row (Maybe Int64))
colFullName    :: (ColumnName, E.Params Project, D.Row Text)
colDisplayName :: (ColumnName, E.Params Project, D.Row Text)
colColor       :: (ColumnName, E.Params Project, D.Row Text)
colPM          :: (ColumnName, E.Params Project, D.Row (Maybe RES.ResourceId))
colCRM         :: (ColumnName, E.Params Project, D.Row (Maybe RES.ResourceId))
colType        :: (ColumnName, E.Params Project, D.Row PT.ProjectTypeId)

colExtId       = (ColumnName "external_id" , contramap externalId  $ E.nullableValue E.int8       , D.nullableValue D.int8)
colFullName    = (ColumnName "full_name"   , contramap fullName    $ E.value E.text               , D.value D.text)
colDisplayName = (ColumnName "display_name", contramap displayName $ E.value E.text               , D.value D.text)
colColor       = (ColumnName "color"       , contramap color       $ E.value E.text               , D.value D.text)
colPM          = (ColumnName "project_mgr" , contramap projectMgr  $ E.nullableValue ID.sqlEncoder, D.nullableValue ID.sqlDecoder)
colCRM         = (ColumnName "account_mgr" , contramap accountMgr  $ E.nullableValue ID.sqlEncoder, D.nullableValue ID.sqlDecoder)
colType        = (ColumnName "project_type", contramap projectType $ E.value ID.sqlEncoder        , D.value ID.sqlDecoder)


cols :: [(ColumnName, E.Params Project)]
cols =  [ tup3to2 colExtId
        , tup3to2 colFullName
        , tup3to2 colDisplayName
        , tup3to2 colColor
        , tup3to2 colPM
        , tup3to2 colCRM
        , tup3to2 colType
        ]




insertQ :: Query Project ProjectId
insertQ = mkQuery (mkInsertStmt tblName (fst <$> cols) <> " RETURNING " `addColName3` colPK <> ";") 
                  (foldMap snd $ cols) 
                  (D.singleRow $ getColDec colPK)
                  True

importQ :: Query Project (Int64, ProjectId)
importQ = mkQuery (mkInsertStmt tblName (fst <$> cols) 
                    <> mkConflictStmt (getColName colExtId) (fst <$> cols)
                    <> " RETURNING " `addColName3` colExtId <> ", " `addColName3` colPK
                    <> ";")  
                  (foldMap snd cols)
                  (D.singleRow $ (,) <$> (fromJust <$> getColDec colExtId) -- external id MUST exist for import
                                     <*> getColDec colPK)
                  True

readRow :: D.Row (ProjectId, Project)
readRow = (,) <$> D.value ID.sqlDecoder
              <*> (Project <$> getColDec colExtId
                           <*> getColDec colFullName
                           <*> getColDec colDisplayName
                           <*> getColDec colColor
                           <*> getColDec colPM
                           <*> getColDec colCRM
                           <*> getColDec colType )



getAllQ :: Query () [(ProjectId, Project)]
getAllQ = mkQuery queryTxt E.unit (D.rowsList readRow) True
  where queryTxt = mkSelectStmt tblName $ getColName colPK : fmap fst cols
                     
