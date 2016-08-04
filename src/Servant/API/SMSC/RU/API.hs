{-|
Module      : Servant.API.SMSC.RU.API
Description : Contains raw servant API for smsc.ru service
Copyright   : (c) Anton Gushcha, 2016
License     : MIT
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Servant.API.SMSC.RU.API(
  -- * API
    SMSCAPI
  -- ** Endpoints
  , SendResponse(..)
  , PhoneResp(..)
  , SendErrorCode(..)
  , SendEndpoint
  ) where 

import Control.Monad
import Data.Aeson
import Data.Monoid 
import Data.Scientific
import Data.Text
import GHC.Generics
import Servant.API 

-- | All supported API of smsc.ru service
type SMSCAPI = SendEndpoint

-- | Error type
data SendErrorCode = 
    SendParametersError
  | SendInvalidLoginPass
  | SendLowFunds
  | SendBlockedDueErrors
  | SendInvalidDateFormat
  | SendForbiddenMessage
  | SendInvalidPhoneFormat
  | SendMessageCannotBeDelivered
  | SendTooMuchMessages
  deriving (Generic, Show, Eq, Bounded, Enum)

-- | Convert from received error code
fromErrorCode :: Word -> Maybe SendErrorCode
fromErrorCode w = case w of 
  1 -> Just SendParametersError
  2 -> Just SendInvalidLoginPass
  3 -> Just SendLowFunds
  4 -> Just SendBlockedDueErrors
  5 -> Just SendInvalidDateFormat
  6 -> Just SendForbiddenMessage
  7 -> Just SendInvalidPhoneFormat
  8 -> Just SendMessageCannotBeDelivered
  9 -> Just SendTooMuchMessages
  _ -> Nothing

instance FromJSON SendErrorCode where 
  parseJSON (Number s) = case fromErrorCode =<< toBoundedInteger s of 
    Nothing -> fail $ "unknown error code " <> show s
    Just c -> return c 
  parseJSON _ = mzero

-- | Additional info about phone number
data PhoneResp = PhoneResp {
  phoneNumber :: !Text 
, phoneMccmnc :: !Text 
, phoneCost :: !Text 
, phoneStatus :: !Text 
, phoneError :: !Text
} deriving (Generic, Show)

instance FromJSON PhoneResp where 
  parseJSON (Object o) = PhoneResp 
    <$> o .: "phone"
    <*> o .: "mccmnc"
    <*> o .: "cost"
    <*> o .: "status"
    <*> o .: "error"
  parseJSON _ = mzero

-- | Response for 'SendEndpoint'
data SendResponse = 
    -- | Server returned error payload
    SendError {
      sendError :: !Text -- ^ Desciption of error
    , sendErrorCode :: !SendErrorCode -- ^ Error code
    , sendErrorId :: !(Maybe Text) -- ^ ID of failed message
    }
    -- | Server retunred success payload
  | SendSuccess {
      sendSuccId :: !(Maybe Word) -- ^ ID of sended message
    , sendSuccCnt :: !Word -- ^ Count of messages sended (huge payloads are splitted)
    , sendSuccCost :: !(Maybe Text) -- ^ Cost of sending
    , sendSuccBalance :: !(Maybe Text) -- ^ Funds of the account left after the call
    , sendSuccPhones :: !(Maybe [PhoneResp]) -- ^ Additional info by phone number
    }
  deriving (Generic, Show)

instance FromJSON SendResponse where 
  parseJSON (Object o) = do 
    merr <- o .:? "error"
    case merr of 
      Nothing -> SendSuccess
        <$> o .:? "id"
        <*> o .: "cnt"
        <*> o .:? "cost"
        <*> o .:? "balance"
        <*> o .:? "phones"
      Just err -> SendError err 
        <$> o .: "error_code"
        <*> o .:? "id"
  parseJSON _ = mzero
  
-- | Endpoint for sending sms, this is low-level most general 
-- API that is used to built small helper functions.
type SendEndpoint = "sys" :> "send.php"
  :> QueryParam "login" Text
  :> QueryParam "psw" Text 
  :> QueryParam "phones" Text
  :> QueryParam "mes" Text 
  -- additional parameters
  :> QueryParam "id" Text 
  :> QueryParam "sender" Text 
  :> QueryParam "translit" Text
  :> QueryParam "tinyurl" Text 
  :> QueryParam "time" Text 
  :> QueryParam "tz" Text 
  :> QueryParam "period" Double
  :> QueryParam "freq" Word
  :> QueryParam "flash" Word 
  :> QueryParam "bin" Text 
  :> QueryParam "push" Word 
  :> QueryParam "hlr" Word 
  :> QueryParam "ping" Word 
  :> QueryParam "mms" Word 
  :> QueryParam "mail" Word 
  :> QueryParam "call" Word 
  :> QueryParam "voice" Text 
  :> QueryParam "param" Text 
  :> QueryParam "subj" Text 
  :> QueryParam "charset" Text 
  :> QueryParam "cost" Word 
  :> QueryParam "fmt" Word -- should be always 3 
  :> QueryParam "list" Text 
  :> QueryParam "valid" Text 
  :> QueryParam "maxsms" Word 
  :> QueryParam "imgcode" Text 
  :> QueryParam "userip" Text 
  :> QueryParam "err" Word 
  :> QueryParam "op" Word 
  :> QueryParam "pp" Text
  :> Post '[JSON] SendResponse