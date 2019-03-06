{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module NPT.WebAPI.Types where

import NPT.WebAPI.Instances ()
import NPT.WebAPI.AuthTypes (Creds, AuthTokenData, AuthToken)


import Data.Time.Calendar (Day)


import Servant ( (:>)
               , (:<|>)
               , JSON
               , PlainText
               , Get
               , Post
               , Delete
               , Capture
               , ReqBody
               , Raw
               )

import Servant.Auth.Server (JWT, Auth)



import qualified NPT.Data as DAT
import qualified NPT.Data.Resource as RS
import qualified NPT.Data.GroupType as GRPT
import qualified NPT.Data.Group as GRP
import qualified NPT.Data.Project as PRJ 
import qualified NPT.Data.ProjectType as PT
import qualified NPT.Data.WorkOrder as WOR
import qualified NPT.Data.Booking as BOK
import qualified NPT.Data.ResourcePlanningTable as RPT
import qualified NPT.Data.WorkOrdersTable as WOT
import qualified NPT.Data.WorkOrdersExecutiveTable as WOXT


-------- APIs ----------------------


type NPTAPI = NPTWebAPI :<|> NPTHomePage :<|> NPTWebsite


type NPTWebsite = Raw


type NPTHomePage = Get '[PlainText] String


type PostJSON a b = ReqBody '[JSON] a :> Post '[JSON] b


type NPTWebAPI = "api" :> "v1" :> ( LoginAPI :<|> ( Auth '[JWT] AuthTokenData :> RequiresLoginAPI ) )


type LoginAPI = "login" :> PostJSON Creds AuthToken


type RequiresLoginAPI =  ResourcesAPI
                    :<|> ProjectsAPI 
                    :<|> ProjectTypesAPI
                    :<|> GroupTypesAPI
                    :<|> GroupsAPI
                    :<|> WorkOrdersAPI 
                    :<|> BookingsAPI
                    :<|> PlanningTablesAPI




type ResourcesAPI = "resources" :> Get '[JSON] [DAT.KeyedObject RS.Resource]


type ProjectsAPI = "projects" :> (    Get '[JSON] [DAT.KeyedObject PRJ.Project]
                                 :<|> PostJSON PRJ.Project PRJ.ProjectId
                                 )
                                 

type ProjectTypesAPI = "projecttypes" :> Get '[JSON] [DAT.KeyedObject PT.ProjectType]


type GroupTypesAPI = "grouptypes" :> Get '[JSON] [DAT.KeyedObject GRPT.GroupType]


type GroupsAPI = "groups" :> Get '[JSON] [DAT.KeyedObject GRP.Group]


type WorkOrdersAPI = "workorders" :> (    Get '[JSON] [DAT.KeyedObject WOR.WorkOrder]
                                     :<|> PostJSON WOR.WorkOrder WOR.WorkOrderId
                                     :<|> Capture "workOrderId" WOR.WorkOrderId :> PostJSON WOR.WorkOrder ()
                                     :<|> Capture "workOrderId" WOR.WorkOrderId :> Delete '[JSON] () 
                                     )




type BookingsAPI = "bookings" :> (    Capture "startDate" Day :> Capture "endDate" Day :> Get '[JSON] [DAT.KeyedObject BOK.Booking]
                                 :<|> Capture "bookingId" BOK.BookingId :> PostJSON BOK.Booking ()
                                 :<|> Capture "bookingId" BOK.BookingId :> Delete '[JSON] ()
                                 :<|> PostJSON BOK.Booking BOK.BookingId
                                 )




type PlanningTablesAPI = "tables" :> ( ( "resources" :> Capture "startDate" Day
                                                     :> Capture "endDate" Day
                                                     :> Get '[JSON] [RPT.PlanResRow]
                                       )
                                       :<|>
                                       ( "workorders" :> Capture "startDate" Day
                                                      :> Capture "endDate" Day
                                                      :> Get '[JSON] [WOT.WORow]
                                       )
                                       :<|>
                                       ( "workordersexec" :> Capture "startDate" Day
                                                          :> Capture "endDate" Day
                                                          :> PostJSON [GRP.GroupId] [WOXT.WOExRow]
                                       )
                                     )











