{-# LANGUAGE DeriveGeneric #-}

module NPT.Data.DataId where


import Data.Foldable (foldl')
import Data.Functor.Contravariant (contramap)
import Data.Int (Int64)

import GHC.Generics (Generic)

import Text.Read (readPrec)

import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D


newtype DataId a = DataId { runId :: Int64 
                          }
                          deriving (Show, Eq, Generic)


instance Read (DataId a) where
  readPrec = fmap DataId readPrec


sqlEncoder :: E.Value (DataId a)
sqlEncoder = contramap runId E.int8


sqlEncoderList :: E.Value [DataId a]
sqlEncoderList = E.array $ E.arrayDimension foldl' $ E.arrayValue sqlEncoder


sqlDecoder :: D.Value (DataId a)
sqlDecoder = DataId <$> D.int8


