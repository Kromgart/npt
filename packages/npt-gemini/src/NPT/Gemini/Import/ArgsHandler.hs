module NPT.Gemini.Import.ArgsHandler where


import NPT.Gemini.Internal ( ImportError
                           , ImportError (NoArgs)
                           , ImportError (ParseError)
                           , mapLeft
                           )

import NPT.Gemini.Configuration (Config, dbSettings)

import NPT.Data.LoggedTime (getLastImportTsmp)
import NPT.Data ( DBSettings
                , runSingleQ
                , mkConnectionString
                )


import Control.Monad.Trans.Except ( withExceptT
                                  , catchE
                                  , throwE)

import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Data.Time.Clock (UTCTime)
import Text.Read (readEither) 
import System.Environment (getArgs)


getTimeParameter :: Config -> ExceptT ImportError IO UTCTime
getTimeParameter cfg = catchE getTimeFromArgs (getTimeFromDb $ dbSettings cfg)


getTimeFromDb :: DBSettings -> ImportError -> ExceptT ImportError IO UTCTime
getTimeFromDb cfg (NoArgs _) = withExceptT cerr $ runSingleQ (mkConnectionString cfg) getLastImportTsmp ()
  where cerr = NoArgs . (++) "Import start date is missing both from program arguments and database; " . show 
getTimeFromDb _ x = throwE x 


getTimeFromArgs :: ExceptT ImportError IO UTCTime
getTimeFromArgs = ExceptT $ tryToUtc <$> getArgs


tryToUtc :: [String] -> Either ImportError UTCTime
tryToUtc [] = mapLeft NoArgs $ Left "Arguments missing: import from which date?"
tryToUtc (h : _) = fpe $ readEither h
  where fpe (Left e) = mapLeft ParseError $ Left e
        fpe (Right x)  = Right x 
