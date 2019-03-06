{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module NPT.BambooHR.Import.Resources where

import NPT.BambooHR.Internal ( ConversionError (ConversionFieldError, ConversionOtherError)
                             , IdsToExtMap
                             , FieldError
                             , rdOptM
                             , rdReq
                             , rdReqStringM
                             , mapLeft
                             )

import qualified NPT.BambooHR.Types as T
import qualified NPT.Data as DAT
import qualified NPT.Data.DataId as ID
import qualified NPT.Data.Resource as RS
import qualified NPT.Data.Group as GRP

import Control.Arrow (returnA)
import Data.List  (find)
import Data.Maybe (maybeToList)
import Data.Text  (Text)
import qualified Data.Text as TXT



checkEndDate :: Text -> Maybe Text
checkEndDate x | x /= "0000-00-00" = Just x 
               | otherwise = Nothing


convertToResource :: [(GRP.GroupId, GRP.Group)] -> T.EmployeeDef -> Either (ConversionError T.EmployeeDef) RS.Resource
convertToResource locs e = do extId <- ferr $ fmap Just $ rdReq "ResourceId" $ T.empId e
                              sudId <- ferr $ rdOptM "SupervisorId" $ T.empSupervisorId e
                              sDate <- ferr $ rdReq "StartDate" $ T.empStartDate e
                              eDate <- ferr $ rdOptM "EndDate" $ T.empEndDate e >>= checkEndDate 
                              title <- ferr $ rdReqStringM "Title" $ T.empTitle e
                              eLoc  <- (ferr $ rdReqStringM "Location" $ T.empLocation e) >>= getGroup
                              let resName = T.empName e
                                  schId = ID.DataId 1
                                  groups = maybeToList eLoc
                                  email = T.empEmail e
                                  phone = T.empPhone e
                              
                              mapLeft (ConversionOtherError e) $ RS.mkResource extId sudId resName schId sDate eDate title email phone groups
                             
                            
                           

  where ferr :: Either FieldError a -> Either (ConversionError T.EmployeeDef) a
        ferr = mapLeft (ConversionFieldError e)

        getGroup :: Text -> Either (ConversionError T.EmployeeDef) (Maybe GRP.GroupId)
        getGroup locTxt 
          | TXT.null locTxt = Right Nothing
          | otherwise = let loc = fst <$> find ((== locTxt) . GRP.displayName . snd) locs in
                        case loc of Nothing -> Left $ ConversionOtherError e $ "Cannot find group '" ++ show locTxt ++ "' in a list of provided groups"
                                    x -> Right x


type ResIdExtMap = IdsToExtMap RS.Resource

type SupervisorsMap = [(RS.ResourceId, Maybe RS.ResourceId)]


importResourcesTQ :: DAT.TransQuery [RS.Resource] ResIdExtMap
importResourcesTQ = proc rs -> do DAT.TransQuery RS.resetEmailsQ -< ()
                                  idsmap <- DAT.mkBatchA RS.importTQ -< rs
                                  DAT.TransQuery (DAT.mkBatchA RS.setSupervisorQ) -< getSups rs idsmap
                                  returnA -< idsmap

  where getSups :: [RS.Resource] -> ResIdExtMap -> SupervisorsMap
        getSups rs ids = foldr f [] rs
          where f :: RS.Resource -> SupervisorsMap -> SupervisorsMap
                f r acc = let mp = do extId <- RS.externalId r
                                      newId <- fst <$> find (\(_,y) -> y == extId) ids
                                      let newSup = RS.supervisor r >>= (\(ID.DataId si) -> fst <$> find (\(_,y) -> y == si) ids)
                                      return (newId, newSup)

                          in case mp of Nothing -> acc
                                        Just p -> p : acc


 


