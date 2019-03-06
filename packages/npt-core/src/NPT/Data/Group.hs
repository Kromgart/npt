{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Arrows #-}

module NPT.Data.Group where

import qualified NPT.Data.DataId as ID
import qualified NPT.Data.GroupType as GT
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
 


data Group = Group { displayName :: Text
                   , groupTypeId :: GT.GroupTypeId
                   }
                   deriving (Generic, Show)


type GroupId = ID.DataId Group


colPK     :: (ColumnName, E.Params GroupId, D.Row GroupId)
colName   :: (ColumnName, E.Params Group,   D.Row Text)
colType   :: (ColumnName, E.Params Group,   D.Row GT.GroupTypeId)


colPK     = (ColumnName "group_id",      E.value ID.sqlEncoder,                         D.value ID.sqlDecoder)
colName   = (ColumnName "display_name",  contramap displayName $ E.value E.text,        D.value D.text)
colType   = (ColumnName "group_type_id", contramap groupTypeId $ E.value ID.sqlEncoder, D.value ID.sqlDecoder)


tblName :: TableName
tblName = TableName "groups"


readRow :: D.Row (GroupId, Group)
readRow = (,) <$> getColDec colPK
              <*> (Group <$> getColDec colName
                         <*> getColDec colType )


getAllQ :: Query () [(GroupId, Group)]
getAllQ = mkQuery (mkSelectStmt tblName (getColName colPK : getColName colName : getColName colType : []))
                  E.unit
                  (D.rowsList readRow)
                  True

getAllByTypeQ :: Query GT.GroupTypeId [(GroupId, Group)]
getAllByTypeQ = mkQuery ((mkSelectStmt tblName (getColName colPK : getColName colName : getColName colType : []))
                         <> " WHERE " `addColName3` colType <> " = $1;")
                        (E.value ID.sqlEncoder)
                        (D.rowsList readRow)
                        True

importQ :: Query Group (GroupId, Group)
importQ = proc g -> do gid <- insertQ -< g
                       returnA -< (gid, g)

insertQ :: Query Group GroupId
insertQ = mkQuery (mkInsertStmt tblName (getColName colName : getColName colType : []) 
                     <> " RETURNING " `addColName3` colPK
                     <> ";"
                  )
                  (getColEnc colName <> getColEnc colType)
                  (D.singleRow $ getColDec colPK)
                  True


                  



