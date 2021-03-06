{-# LANGUAGE OverloadedStrings #-}

module NPT.Gemini.Configuration where

import qualified NPT.Data as DAT

import Data.Aeson ( FromJSON
                  , parseJSON
                  , withObject
                  , (.:)
                  , eitherDecode')

import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict)
import Data.Text.Encoding (encodeUtf8)

data Config = Config { dbSettings :: DAT.DBSettings
                     , geminiSettings :: GeminiSettings
                     }

data GeminiSettings = GeminiSettings { host  :: String
                                     , port :: Int }

readConfigFile :: FilePath -> IO (Either String Config)
readConfigFile = fmap parseConfig . BS.readFile

parseConfig :: BS.ByteString -> Either String Config
parseConfig = eitherDecode' . fromStrict

instance FromJSON Config where
  parseJSON = withObject "Config" $ 
                          \o -> Config <$> ((o .: "database") >>= dbSettingsParser)
                                       <*> ((o .: "gemini") >>= geminiParser)

    where dbSettingsParser = withObject "DBSettings" $                                     
                                        \o -> let pck = fmap encodeUtf8 . (o .:) in
                                              DAT.DBSettings <$> pck "host"
                                                             <*> o .: "port"
                                                             <*> pck "login"
                                                             <*> pck "password"
                                                             <*> pck "database"
          geminiParser = withObject "GeminiSettings" $
                                    \o -> GeminiSettings <$> o .: "host"
                                                         <*> o .: "port"