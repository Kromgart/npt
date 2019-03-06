{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module NPT.Data.Internal where

import qualified NPT.Data.DataId as ID

import Prelude hiding (id, (.))
import Control.Category ( Category
                        , id
                        , (.)
                        )

import Control.Arrow ( Arrow
                     , ArrowChoice
                     , arr
                     , first
                     , left
                     , returnA
                     )
import Data.Functor.Contravariant (contramap)
import Data.Time.Calendar (Day)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as TXT
import Data.Text.Encoding (encodeUtf8)


import Hasql.Query (Query, statement)
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D
import Hasql.Decoders (Result)


data DateRange = DateRange { dateStart :: Day
                           , dateEnd   :: Day
                           } 
                           deriving Show


dateRangeEncoder :: E.Params DateRange
dateRangeEncoder = contramap dateStart (E.value E.date) <> contramap dateEnd (E.value E.date) 


mkDateRange :: Day -> Day -> Either String DateRange
mkDateRange d1 d2 | d1 > d2 = Left "Error: DateRange startDate cannot be greater than endDate"
                  | otherwise = Right $ DateRange d1 d2


newtype TransQuery a b = TransQuery { getQuery :: Query a b }


instance Category TransQuery where
  id = TransQuery id
  (TransQuery q1) . (TransQuery q2) = TransQuery $ q1 . q2


instance Arrow TransQuery where
  arr = TransQuery . arr
  first (TransQuery q) = TransQuery $ first q


instance ArrowChoice TransQuery where
  left (TransQuery q) = TransQuery $ left q


mkBatchA :: ArrowChoice x => x a b -> x [a] [b]
mkBatchA q = proc ys -> do case ys of [] -> returnA -< []
                                      (x:xs) -> do r1 <- q -< x
                                                   rs <- mkBatchA q -< xs
                                                   returnA -< r1 : rs


newtype ColumnName = ColumnName { runColumnName :: Text }


instance Show ColumnName where
  show = show . runColumnName


addColName :: Text -> ColumnName -> Text
txt `addColName` (ColumnName cn) = txt <> cn

addColName2 :: Text -> (ColumnName, b) -> Text
txt `addColName2` (cn, _) = txt `addColName` cn

addColName3 :: Text -> (ColumnName, b, c) -> Text
txt `addColName3` (cn, _, _) = txt `addColName` cn


newtype TableName = TableName { runTableName :: Text }


instance Show TableName where
  show = show . runTableName


addTable :: Text -> TableName -> Text
txt `addTable` (TableName n) = txt <> n


mkArgsList :: [a] -> Text
mkArgsList = TXT.intercalate ", " . zipWith (const . TXT.cons '$' . TXT.pack . show) ([1 .. ] :: [Int])


mkSelectStmt :: TableName -> [ColumnName] -> Text
mkSelectStmt tbl cs = "SELECT " <> TXT.intercalate ", " (runColumnName <$> cs) <> " FROM " <> runTableName tbl


mkDeleteStmt :: TableName -> ColumnName -> Text
mkDeleteStmt tbl col = "DELETE FROM " `addTable` tbl <> " WHERE " `addColName` col <> " = $1;"


mkInsertStmt :: TableName -> [ColumnName] -> Text
mkInsertStmt tbl cs = "INSERT INTO " <> runTableName tbl <> " ("
                                     <> TXT.intercalate ", " (runColumnName <$> cs)
                                     <> ") VALUES (" <> mkArgsList cs <> ")"


mkUpdateStmt :: TableName -> ColumnName -> [ColumnName] -> Text
mkUpdateStmt tbl colId cols = "UPDATE " <> runTableName tbl <> " SET " <> lst <> " WHERE " `addColName` colId <> " = $1;"
  where lst = TXT.intercalate ", " $ zipWith (\x y -> TXT.pack (show x) <> " = $" <> TXT.pack (show y)) cols ([2 .. ] :: [Int])


mkConflictStmt :: ColumnName -> [ColumnName] -> Text
mkConflictStmt c cs = " ON CONFLICT (" `addColName` c <> ") DO UPDATE SET " 
                                       <> TXT.intercalate ", " ((\x -> let x' = runColumnName x in x' <> " = EXCLUDED." <> x') <$> cs)


mkQuery :: Text -> E.Params a -> Result b -> Bool -> Query a b
mkQuery = statement . encodeUtf8


mkDeleteQuery :: TableName -> ColumnName -> Query (ID.DataId a) ()
mkDeleteQuery t c = mkQuery (mkDeleteStmt t c) 
                            (E.value ID.sqlEncoder)
                            D.unit 
                            False



tup3to2 :: (a, b, c) -> (a, b)
tup3to2 (x, y, _) = (x, y)


getColName :: (a, b, c) -> a
getColName (x, _, _) = x


getColEnc :: (a, b, c) -> b
getColEnc (_, y, _) = y


getColDec :: (a, b, c) -> c
getColDec (_, _, z) = z


