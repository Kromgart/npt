{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module NPT.BambooHR.Types where


import Data.Text (Text)

import Data.Aeson ( FromJSON
                  , parseJSON
                  , withObject
                  , (.:)
                  , (.:?) )

import Servant.API ( (:>)
                   , Header
                   , QueryParam
                   , ReqBody
                   , JSON
                   , PlainText
                   , Post
                   , Get)


type AuthToken = String

type ContentType = String

  
data TimeoffEntry = TimeoffEntry { timeoffId   :: Text
                                 , employeeId  :: Text
                                 , timeoffType :: Text
                                 , status      :: Text
                                 , lastChanged :: Text
                                 , startDate   :: Text
                                 , endDate     :: Text }
                                 deriving Show


instance FromJSON TimeoffEntry where
  parseJSON = withObject "TimeoffEntry" $ 
                          \v -> let st  = v .: "status"
                                    typ = v .: "type"
                                in TimeoffEntry <$> v .: "id"
                                                <*> v .: "employeeId"
                                                <*> (typ >>= withObject "type"   (.: "id"))
                                                <*> (st  >>= withObject "status" (.: "status"))
                                                <*> (st  >>= withObject "status" (.: "lastChanged"))
                                                <*> v .: "start"
                                                <*> v .: "end"


type TimeoffAPI =  "time_off"
                :> "requests"
                :> Header "Authorization" AuthToken
                :> Header "Accept"        ContentType
                :> QueryParam "start"  String
                :> QueryParam "end"    String
                :> QueryParam "status" String
                :> Get '[JSON] [TimeoffEntry]




data EmployeeDef = EmployeeDef { empId           :: Text
                               , empSupervisorId :: Maybe Text
                               , empName         :: Text
                               , empStartDate    :: Text
                               , empEndDate      :: Maybe Text
                               , empTitle        :: Maybe Text
                               , empLocation     :: Maybe Text
                               , empEmail        :: Maybe Text
                               , empPhone        :: Maybe Text }
                               deriving Show


instance FromJSON EmployeeDef where
  parseJSON = withObject "Employee" $ \v -> EmployeeDef <$> v .:  "id"
                                                        <*> v .:? "supervisorEId"
                                                        <*> v .:  "displayName"
                                                        <*> v .:  "hireDate"
                                                        <*> v .:? "terminationDate"
                                                        <*> v .:  "jobTitle"
                                                        <*> v .:  "location"
                                                        <*> v .:? "workEmail"
                                                        <*> v .:? "mobilePhone"


newtype EmployeesList = EmployeesList { runList :: [EmployeeDef] }
                        deriving Show

instance FromJSON EmployeesList where
  parseJSON = withObject "Employees" $ \v -> EmployeesList <$> v .: "employees"


type EmployeesAPI =  "reports"
                  :> "custom"
                  :> Header "Authorization" AuthToken
                  :> Header "Accept"        ContentType
                  :> ReqBody '[PlainText] Text
                  :> Post '[JSON] EmployeesList



