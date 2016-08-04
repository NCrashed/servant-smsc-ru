{-|
Module      : Servant.API.SMSC.RU
Description : Meta module that contains all API for smsc.ru service
Copyright   : (c) Anton Gushcha, 2016
License     : MIT
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable

The module contains general and simplified functions to call <https://smsc.ru smsc.ru> service
for sending SMS/MMS messages.

The general API is provided by 'genericSmsSend' function, but it is too overengineered to be used
as is. Thats why several simplified wrappers are presented.

First, you need to provide 'SmscConfig' value:

@
cfg <- defaultSmscConfig
let testSmscConfig = cfg {
        smscLogin = "mylogin"
      , smscPassword = "mypass"
      }
@

After that you can send a SMS:

@
res <- simpleSmsSend testSmscConfig testPhone "Test message"
case res of 
  Left er -> printLn $ "message sending: " ++ show er
  Right _ -> return ()
@

Or check how much it would cost to you:

@
res <- getSimpleSmsCost testSmscConfig testPhone "Test message"
case res of 
  Left er -> assertFailure $ "message costing: " <> show er
  Right v -> putStrLn $ "message cost: " <> show v
@

-}
module Servant.API.SMSC.RU(
  -- * API
    SMSCAPI
  -- ** Endpoints
  , SendResponse(..)
  , PhoneResp(..)
  , SendEndpoint
  -- * Client
  , genericSmsSend
  -- ** Simplified
  , SmscConfig(..)
  , defaultSmscConfig
  , simpleSmsSend
  , getSimpleSmsCost 
  ) where 

import Control.Monad.Except
import Data.Monoid 
import Data.Proxy
import Data.Text 
import GHC.Generics 
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS
import Servant.API.SMSC.RU.API
import Servant.Client 

import qualified Data.Text as T 

-- | The most generic call to the send endpoint
genericSmsSend :: 
     Maybe Text -- ^ login
  -> Maybe Text  -- ^ psw
  -> Maybe Text -- ^ phones
  -> Maybe Text  -- ^ mes
  -> Maybe Text  -- ^ id
  -> Maybe Text  -- ^ sender
  -> Maybe Text -- ^ translit
  -> Maybe Text  -- ^ tinyurl
  -> Maybe Text  -- ^ time
  -> Maybe Text  -- ^ tz
  -> Maybe Double -- ^ period
  -> Maybe Word -- ^ freq
  -> Maybe Word  -- ^ flash
  -> Maybe Text  -- ^ bin
  -> Maybe Word  -- ^ push
  -> Maybe Word  -- ^ hlr
  -> Maybe Word  -- ^ ping
  -> Maybe Word  -- ^ mms
  -> Maybe Word  -- ^ mail
  -> Maybe Word  -- ^ call
  -> Maybe Text  -- ^ voice
  -> Maybe Text  -- ^ param
  -> Maybe Text  -- ^ subj
  -> Maybe Text  -- ^ charset
  -> Maybe Word  -- ^ cost
  -> Maybe Word -- ^ fmt, should be always 3
  -> Maybe Text  -- ^ list
  -> Maybe Text  -- ^ valid
  -> Maybe Word  -- ^ maxsms
  -> Maybe Text  -- ^ imgcode
  -> Maybe Text  -- ^ userip
  -> Maybe Word  -- ^ err
  -> Maybe Word  -- ^ op
  -> Maybe Text -- ^ pp
  -> Manager -> BaseUrl -> ClientM SendResponse
genericSmsSend = client (Proxy :: Proxy SMSCAPI)

-- | Required common info for smsc API
data SmscConfig = SmscConfig {
  smscLogin :: !Text -- ^ Account login
, smscPassword :: !Text  -- ^ Account password
, smscManager :: !Manager  -- ^ Connection manager (use http-)
, smscBaseUrl :: !BaseUrl
} deriving (Generic)

instance Show SmscConfig where 
  show SmscConfig{..} = "SmscConfig {" 
    <> "smscLogin = " <> show smscLogin
    <> ", smscPassword = " <> show smscPassword
    <> ", smscManager = <object> "
    <> ", smscBaseUrl = " <> show smscBaseUrl 
    <> " }"

defaultSmscConfig :: MonadIO m => m SmscConfig
defaultSmscConfig = do 
  mng <- liftIO $ newManager tlsManagerSettings
  return SmscConfig {
      smscLogin = ""
    , smscPassword = ""
    , smscManager = mng 
    , smscBaseUrl = BaseUrl {
        baseUrlScheme = Https
      , baseUrlHost = "smsc.ru"
      , baseUrlPort = 443
      , baseUrlPath = ""
      }
    }

-- | Simple send message to given phone
simpleSmsSend :: MonadIO m 
  => SmscConfig -- ^ Authorisation and other general config
  -> Text -- ^ Phone
  -> Text -- ^ Message
  -> m (Either Text SendResponse)
simpleSmsSend SmscConfig{..} phone msg = do 
  res <- liftIO . runExceptT $ genericSmsSend
    (Just smscLogin) 
    (Just smscPassword)
    (Just phone) -- phones
    (Just msg) -- mes
    Nothing -- id
    Nothing -- sender
    Nothing -- translit
    Nothing -- tinyurl
    Nothing -- time
    Nothing -- tz
    Nothing -- period
    Nothing -- freq
    Nothing -- flash
    Nothing -- bin
    Nothing -- push
    Nothing -- hlr
    Nothing -- ping
    Nothing -- mms
    Nothing -- mail
    Nothing -- call
    Nothing -- voice
    Nothing -- param
    Nothing -- subj
    Nothing -- charset
    Nothing -- cost
    (Just 3) -- fmt always 3
    Nothing -- list
    Nothing -- valid
    Nothing -- maxsms
    Nothing -- imgcode
    Nothing -- userip
    Nothing -- err
    Nothing -- op
    Nothing -- pp
    smscManager smscBaseUrl
  return $ case res of 
    Left e -> Left (T.pack $ show e)
    Right r -> Right r

-- | Getting cost of message without sending
getSimpleSmsCost :: MonadIO m 
  => SmscConfig -- ^ Authorisation and other general config
  -> Text -- ^ Phone
  -> Text -- ^ Message
  -> m (Either Text Text)
getSimpleSmsCost SmscConfig{..} phone msg = do 
  res <- liftIO . runExceptT $ genericSmsSend
    (Just smscLogin) 
    (Just smscPassword)
    (Just phone) -- phones
    (Just msg) -- mes
    Nothing -- id
    Nothing -- sender
    Nothing -- translit
    Nothing -- tinyurl
    Nothing -- time
    Nothing -- tz
    Nothing -- period
    Nothing -- freq
    Nothing -- flash
    Nothing -- bin
    Nothing -- push
    Nothing -- hlr
    Nothing -- ping
    Nothing -- mms
    Nothing -- mail
    Nothing -- call
    Nothing -- voice
    Nothing -- param
    Nothing -- subj
    Nothing -- charset
    (Just 1) -- cost
    (Just 3) -- fmt always 3
    Nothing -- list
    Nothing -- valid
    Nothing -- maxsms
    Nothing -- imgcode
    Nothing -- userip
    Nothing -- err
    Nothing -- op
    Nothing -- pp
    smscManager smscBaseUrl
  return $ case res of 
    Left e -> Left (T.pack $ show e)
    Right r -> case r of 
      se@SendError{} -> Left (T.pack $ show se)
      SendSuccess{..} -> case sendSuccCost of 
        Nothing -> Left "No cost in response"
        Just c -> return c
