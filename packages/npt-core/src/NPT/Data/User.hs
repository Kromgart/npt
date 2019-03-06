{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module NPT.Data.User where

import qualified NPT.Data.DataId as ID

import Control.Arrow (returnA)
import Data.Text (Text)
import GHC.Generics (Generic)

import Hasql.Query (Query)


{- Instances for Aeson are orphaned in respective executables -}

type Username = Text
type Password = Text

data User = User { username :: Username
                 , email    :: Text
                 }
                 deriving (Show, Generic)


type UserId = ID.DataId User



loginUserQ :: Query (Username, Password) (Maybe (UserId, User))
loginUserQ = proc (u, p) -> do let i = if u == "kromgart" then Just (ID.DataId 1, User u "kromgart@gmail.com")
                                                          else Nothing
                               returnA -< i

