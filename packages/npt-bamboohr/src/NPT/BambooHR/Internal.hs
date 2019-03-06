{-# LANGUAGE OverloadedStrings #-}

module NPT.BambooHR.Internal where

import qualified NPT.BambooHR.Client as CL
import qualified NPT.Data as DAT
import qualified NPT.Data.DataId as ID

import Control.Monad.Trans.Except (ExceptT)
import Data.Int (Int64)
import Data.Text (Text, unpack)
import Text.Read (Read, readEither)


data FieldError = FieldMissing String
                | FieldInvalid String
                deriving Show


data ConversionError a = ConversionFieldError a FieldError
                       | ConversionOtherError a String
                         deriving Show


data ImportError = BambooError CL.RequestError
                 | DBError DAT.DataError
                 | OtherError String
                 deriving Show


type ImportResult a = ExceptT ImportError IO a


type IdsToExtMap a = [(ID.DataId a, Int64)]


mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x


rdReq :: Read a => String -> Text -> Either FieldError a
rdReq fld x = rdReqM fld (Just x)


rdReqM :: Read a => String -> Maybe Text -> Either FieldError a
rdReqM fld Nothing = Left $ FieldMissing fld
rdReqM fld (Just x) =  mapLeft (FieldInvalid . (\s -> fld ++ ": " ++ s ++"; ")) $ readEither $ unpack x


rdOptM :: Read a => String -> Maybe Text -> Either FieldError (Maybe a)
rdOptM _ Nothing = Right Nothing
rdOptM fld x = Just <$> rdReqM fld x


rdReqStringM :: String -> Maybe Text -> Either FieldError Text
rdReqStringM fld Nothing = Left $ FieldMissing fld
rdReqStringM _ (Just x) = Right x






