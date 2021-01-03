{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module UnsafeIO where

import Control.Monad.Trans (MonadIO (liftIO))
import Data.Aeson ()
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Servant

type UnsafeApi =
  "add" :> Get '[JSON] ()
    :<|> "healthcheck" :> Get '[JSON] ()

healthcheck :: IORef Int -> Handler ()
healthcheck ref = do
  t <- liftIO $ readIORef ref
  case t of
    0 -> pure ()
    n -> throwError $ err500 {errBody = "observed inconsistency: " <> (BL8.pack $ show n)}

makeServer :: IO (Server UnsafeApi)
makeServer = do
  ref <- newIORef 0
  pure $
    unsafeMunge ref
      :<|> healthcheck ref

unsafeMunge :: IORef Int -> Handler ()
unsafeMunge ref = liftIO $ do
  t <- readIORef ref
  writeIORef ref (t + 1)
  t2 <- readIORef ref
  writeIORef ref (t2 -1)
