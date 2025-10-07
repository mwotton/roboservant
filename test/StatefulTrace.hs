{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module StatefulTrace
  ( Api,
    server,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, atomicModifyIORef')
import Servant

-- | A simple endpoint that increments a shared counter on every call.
type Api = "count" :> Get '[JSON] Int

server :: IORef Int -> Server Api
server counter = liftIO $ atomicModifyIORef' counter $ \n ->
  let next = n + 1
   in (next, next)
