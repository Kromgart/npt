{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module NPT.Gemini.Types where

import Data.Int (Int64, Int16)
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)
import Servant.API ( (:>)
                   , (:<|>)
                   , JSON
                   , ReqBody
                   , Get
                   , Post)


data ProjectInfo = ProjectInfo { projectId   :: Int64
                               , projectType :: Text
                               , projectCode :: Text
                               , projectName :: Text
                               , color       :: Text
                               } 
                               deriving (Eq, Show, Generic)


instance FromJSON ProjectInfo


data TimeReport = TimeReport { reportId :: Int64
                             , loggedTime :: Int16
                             , entryDate :: Day
                             , project :: Int64
                             , emailAddress :: Text
                             } 
                             deriving (Eq, Show, Generic)


instance FromJSON TimeReport


data TimeReportRequest = TimeReportRequest { timeStamp :: String
                                           , emails    :: [Text]
                                           } 
                                           deriving Generic


instance ToJSON TimeReportRequest


type ProjectsApi = "api/gemini" :> (     "getallprojects" :> Get '[JSON] [ProjectInfo] 
                                    :<|> "gettimereports" :> ReqBody '[JSON] TimeReportRequest :> Post '[JSON] [TimeReport] 
                                   )


