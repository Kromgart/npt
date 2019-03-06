module NPT.Gemini.Import.Projects where

import qualified NPT.Gemini.Internal as GI
import qualified NPT.Gemini.Client as CL
import qualified NPT.Gemini.Types as T
import NPT.Gemini.Configuration (GeminiSettings)

import qualified NPT.Data.Project as PRJ
import qualified NPT.Data.ProjectType as PT

import Data.List (find)
import Data.Text (Text) 
import qualified Data.Text as TXT
import Control.Monad.Trans.Except (withExceptT)


type ProjectIdExtMap = GI.IdsToExtMap PRJ.Project

type ProjectTypeIdExtMap = [(PT.ProjectTypeId, Text)]


convertToProject :: ProjectTypeIdExtMap -> T.ProjectInfo -> Either (GI.ConversionError T.ProjectInfo) PRJ.Project
convertToProject ptids pinf@(T.ProjectInfo pid pt code name color) = 
  case find ((== pt) . snd) ptids of 
    Nothing -> Left $ GI.ConversionOtherError pinf $ "Project type not found: " ++ (show pt) 
    Just (x,_) ->  Right $ PRJ.Project (Just pid) code name (chColor color) Nothing Nothing x

  where chColor ::  Text -> Text
        chColor a | TXT.null a = TXT.pack "#444444" 
                  | otherwise = a


getGeminiProjects :: GeminiSettings -> GI.ImportResult [T.ProjectInfo]
getGeminiProjects cfg = withExceptT GI.GeminiError $ CL.getProjects cfg
