module NPT.Gemini.Import.TimeReports where

import qualified NPT.Gemini.Internal as GI
import qualified NPT.Data.LoggedTime as LT
import qualified NPT.Gemini.Client as CL
import qualified NPT.Gemini.Types as T
import NPT.Gemini.Configuration (GeminiSettings)
import Control.Monad.Trans.Except (withExceptT)


type TrIdExtMap = GI.IdsToExtMap LT.LoggedTime

getTimeReports :: GeminiSettings -> T.TimeReportRequest -> GI.ImportResult [T.TimeReport]
getTimeReports cfg t = withExceptT GI.GeminiError $ CL.getTimeReports cfg t 
