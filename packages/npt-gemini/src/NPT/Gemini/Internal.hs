{-# LANGUAGE OverloadedStrings #-}

module NPT.Gemini.Internal where

import qualified NPT.Data.DataId as ID
import qualified Servant.Client as SC
import qualified NPT.Data as DAT
import Control.Monad.Trans.Except (ExceptT)
import Data.Int (Int64)


type RequestResult a = ExceptT RequestError IO a


type ImportResult a = ExceptT ImportError IO a


type IdsToExtMap a = [(Int64, ID.DataId a)]


data RequestError = InvalidArgs String
                  | SrvError SC.ServantError
                  deriving Show


data ImportError = GeminiError RequestError 
                 | DBError DAT.DataError
                 | NoArgs String
                 | ParseError String
                 | OtherError String
                 deriving Show


data ConversionError a = ConversionOtherError a String
                         deriving Show


withError :: String -> a -> Maybe b -> Either (ConversionError a) b
withError _ _ (Just x) = Right x
withError text tr Nothing = Left (ConversionOtherError tr text)


mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x
