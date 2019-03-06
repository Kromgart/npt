{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module NPT.WebAPI.AuthTypes where

import NPT.WebAPI.Instances ()

import Data.Text (Text)
import GHC.Generics (Generic)

import Data.Aeson ( ToJSON
                  , toEncoding
                  , genericToEncoding
                  , defaultOptions
                  , FromJSON
                  )

import Servant.Auth.Server (FromJWT, ToJWT)



import qualified NPT.Data.DataId as ID
import qualified NPT.Data.User as USR


------ Authentication types ---------

data Creds = Creds { login    :: Text
                   , password :: Text
                   }
                   deriving Generic

instance FromJSON Creds


newtype AuthTokenData = AuthTokenData USR.UserId
                        deriving Generic


instance FromJSON AuthTokenData
instance ToJSON AuthTokenData where
  toEncoding = genericToEncoding defaultOptions

instance FromJWT AuthTokenData
instance ToJWT AuthTokenData


instance FromJWT (ID.DataId a)
instance ToJWT (ID.DataId a)


type AuthToken = Text


