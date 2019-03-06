{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module NPT.Data.GroupType where

import qualified NPT.Data.DataId as ID
import NPT.Data.Internal ( mkQuery
                         , mkSelectStmt
                         , ColumnName (ColumnName)
                         , TableName (TableName)
                         , getColName
                         , getColDec
                         )


import Data.Functor.Contravariant (contramap)
import Data.Text (Text)

import GHC.Generics (Generic)

import Hasql.Query (Query)
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
 


data GroupType = GroupType { displayName :: Text
                           , isLocked :: Bool
                           }
                           deriving (Generic, Show)


type GroupTypeId = ID.DataId GroupType


locationGroupTypeId :: GroupTypeId
locationGroupTypeId = ID.DataId 1


colPK     :: (ColumnName, E.Params GroupTypeId, D.Row GroupTypeId)
colName   :: (ColumnName, E.Params GroupType,   D.Row Text)
colLocked :: (ColumnName, E.Params GroupType,   D.Row Bool)


colPK     = (ColumnName "group_type_id", E.value ID.sqlEncoder,                  D.value ID.sqlDecoder)
colName   = (ColumnName "display_name",  contramap displayName $ E.value E.text, D.value D.text)
colLocked = (ColumnName "is_locked",     contramap isLocked    $ E.value E.bool, D.value D.bool)


tblName :: TableName
tblName = TableName "group_types"


readRow :: D.Row (GroupTypeId, GroupType)
readRow = (,) <$> getColDec colPK
              <*> (GroupType <$> getColDec colName
                             <*> getColDec colLocked )


getAllQ :: Query () [(GroupTypeId, GroupType)]
getAllQ = mkQuery (mkSelectStmt tblName (getColName colPK : getColName colName : getColName colLocked : []))
                  E.unit
                  (D.rowsList readRow)
                  True


