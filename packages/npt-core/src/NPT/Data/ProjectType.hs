{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Arrows #-}

module NPT.Data.ProjectType where

import qualified NPT.Data.DataId as ID
import NPT.Data.Internal ( mkQuery
                         , mkSelectStmt
                         , mkInsertStmt
                         , ColumnName (ColumnName)
                         , TableName (TableName)
                         , getColName
                         , getColEnc
                         , getColDec
                         , addColName3
                         )


import Control.Arrow (returnA)
import Data.Functor.Contravariant (contramap)
import Data.Monoid ((<>))
import Data.Text (Text)

import GHC.Generics (Generic)

import Hasql.Query (Query)
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
 


newtype ProjectType = ProjectType { displayName :: Text }
                                  deriving (Generic, Show)


type ProjectTypeId = ID.DataId ProjectType

tblName :: TableName
tblName = TableName "project_types"


colPK     :: (ColumnName, E.Params ProjectTypeId, D.Row ProjectTypeId)
colName   :: (ColumnName, E.Params ProjectType,   D.Row Text)


colPK     = (ColumnName "project_type_id", E.value ID.sqlEncoder,                  D.value ID.sqlDecoder)
colName   = (ColumnName "display_name",   contramap displayName $ E.value E.text, D.value D.text)



readRow :: D.Row (ProjectTypeId, ProjectType)
readRow = (,) <$> getColDec colPK
              <*> (ProjectType <$> getColDec colName)


getAllQ :: Query () [(ProjectTypeId, ProjectType)]
getAllQ = mkQuery qTxt E.unit (D.rowsList readRow) True
  where qTxt = mkSelectStmt tblName $ getColName colPK : getColName colName : []


importQ :: Query ProjectType (ProjectTypeId, ProjectType)
importQ = proc g -> do gid <- insertQ -< g
                       returnA -< (gid, g)


insertQ :: Query ProjectType ProjectTypeId
insertQ = mkQuery (mkInsertStmt tblName [getColName colName]
                     <> " RETURNING " `addColName3` colPK
                     <> ";"
                  )
                  (getColEnc colName)
                  (D.singleRow $ getColDec colPK)
                  True


                  



