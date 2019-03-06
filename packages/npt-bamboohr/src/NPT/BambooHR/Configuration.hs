{-# LANGUAGE OverloadedStrings #-}

module NPT.BambooHR.Configuration where

import qualified NPT.Data as DAT

import Data.Aeson ( FromJSON
                  , parseJSON
                  , withObject
                  , (.:)
                  , eitherDecode'
                  )

import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict)

import Data.Text.Encoding (encodeUtf8)



data Config = Config { dbSettings :: DAT.DBSettings
                     , bambooSettings :: BambooSettings
                     }


data BambooSettings = BambooSettings { bbHostname  :: String
                                     , bbCompany   :: String
                                     , bbAuthToken :: String
                                     }


readConfigFile :: FilePath -> IO (Either String Config)
readConfigFile = fmap parseConfig . BS.readFile


parseConfig :: BS.ByteString -> Either String Config
parseConfig = eitherDecode' . fromStrict


instance FromJSON Config where
  parseJSON = withObject "Config" $ 
                          \o -> Config <$> ((o .: "database") >>= dbSettingsParser)
                                       <*> ((o .: "bamboohr") >>= bambooParser)

    where dbSettingsParser = withObject "DBSettings" $                                     
                                        \o -> let pck = fmap encodeUtf8 . (o .:) in
                                              DAT.DBSettings <$> pck "host"
                                                             <*> o .: "port"
                                                             <*> pck "login"
                                                             <*> pck "password"
                                                             <*> pck "database"
          bambooParser = withObject "BambooSettings" $
                                    \o -> BambooSettings <$> o .: "hostname"
                                                         <*> o .: "company"
                                                         <*> o .: "authToken"


                    
