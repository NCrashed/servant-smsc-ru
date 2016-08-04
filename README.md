# servant-smsc-ru

[![Build Status](https://travis-ci.org/NCrashed/servant-smsc-ru.svg?branch=master)](https://travis-ci.org/NCrashed/servant-smsc-ru)

These are bindings to https://smsc.ru service for sending SMS/MMS message to cell phones.

# How to use

The general API is provided by `genericSmsSend` function, but it is too overengineered to be used
as is. Thats why several simplified wrappers are presented.

First, you need to provide `SmscConfig` value:

``` haskell
cfg <- defaultSmscConfig
let testSmscConfig = cfg {
        smscLogin = "mylogin"
      , smscPassword = "mypass"
      }
```

After that you can send a SMS:

``` haskell
res <- simpleSmsSend testSmscConfig testPhone "Test message"
case res of 
  Left er -> printLn $ "message sending: " ++ show er
  Right _ -> return ()
```

Or check how much it would cost to you:

``` haskell
res <- getSimpleSmsCost testSmscConfig testPhone "Test message"
case res of 
  Left er -> assertFailure $ "message costing: " <> show er
  Right v -> putStrLn $ "message cost: " <> show v
```