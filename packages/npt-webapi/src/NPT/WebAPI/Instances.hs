{-# OPTIONS_GHC -fno-warn-orphans #-}

module NPT.WebAPI.Instances where

import Data.Aeson ( ToJSON
                  , toJSON
                  , toEncoding
                  , genericToEncoding
                  , defaultOptions
                  , FromJSON
                  , parseJSON
                  )

import Servant (FromHttpApiData, parseUrlPiece)

import qualified NPT.Data.DataId as ID
import qualified NPT.Data.Resource as RS
import qualified NPT.Data.Project as PRJ 
import qualified NPT.Data.WorkOrder as WOR
import qualified NPT.Data.Booking as BOK
import qualified NPT.Data.ResourcePlanningTable as RPT
import qualified NPT.Data.WorkOrdersTable as WOT
import qualified NPT.Data.WorkOrdersExecutiveTable as WOXT
import qualified NPT.Data.GroupType as GRPT
import qualified NPT.Data.Group as GRP
import qualified NPT.Data.ProjectType as PT


instance ToJSON (ID.DataId a) where
  toJSON = toJSON . ID.runId
  toEncoding = toEncoding . ID.runId

instance FromJSON (ID.DataId a) where
  parseJSON = fmap ID.DataId . parseJSON

instance FromHttpApiData (ID.DataId a) where
  parseUrlPiece = fmap ID.DataId . parseUrlPiece



instance ToJSON RS.Resource where
  toEncoding = genericToEncoding defaultOptions



instance FromJSON PRJ.Project

instance ToJSON PRJ.Project where
  toEncoding = genericToEncoding defaultOptions



instance FromJSON WOR.WorkOrder

instance ToJSON WOR.WorkOrder where
  toEncoding = genericToEncoding defaultOptions



instance ToJSON RPT.PlanResRow where
  toEncoding = genericToEncoding defaultOptions



instance ToJSON RPT.PlanResProjectRow where
  toEncoding = genericToEncoding defaultOptions



instance ToJSON RPT.PlanResBooking where
  toEncoding = genericToEncoding defaultOptions



instance ToJSON RPT.PlanResCapacity where
  toEncoding = genericToEncoding defaultOptions



instance ToJSON BOK.Booking where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON BOK.Booking



instance ToJSON WOT.WORow where
  toEncoding = genericToEncoding defaultOptions



instance ToJSON WOXT.WOExGroup where
  toEncoding = genericToEncoding defaultOptions


instance ToJSON WOXT.WOExWorkOrder where
  toEncoding = genericToEncoding defaultOptions


instance ToJSON WOXT.WOExRow where
  toEncoding = genericToEncoding defaultOptions



instance ToJSON GRPT.GroupType where
  toEncoding = genericToEncoding defaultOptions


instance ToJSON GRP.Group where
  toEncoding = genericToEncoding defaultOptions



instance ToJSON PT.ProjectType where
  toEncoding = genericToEncoding defaultOptions




