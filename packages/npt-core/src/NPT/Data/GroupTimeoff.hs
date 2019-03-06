module NPT.Data.GroupTimeoff where


import Data.Text (Text)
import Data.Time.Calendar (Day)

data GroupTimeoff = GroupTimeoff { timeoffId  :: Int
                                 , groupId    :: Int
                                 , title      :: Text
                                 , startDate  :: Day
                                 , endDate    :: Day }
                                 deriving Show

