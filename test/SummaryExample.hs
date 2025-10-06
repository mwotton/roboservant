{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module SummaryExample where

import Servant

-- | Minimal API with a required query parameter and JSON body so we can
-- exercise call summary rendering in tests.
type Api =
  "summary" :> QueryParam' '[Required] "id" Int :> ReqBody '[JSON] Int :> Post '[JSON] Int

server :: Server Api
server ident payload = pure (ident + payload)
