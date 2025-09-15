{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Records where

import Data.Void (Void)
import Servant
import Servant.API.Generic
import Servant.Server.Generic

data Routes mode = Routes
  { getInt :: mode :- Get '[JSON] Int
  , captureVoid :: mode :- Capture "void" Void :> Get '[JSON] ()
  }
  deriving stock (Generic)

type Api = NamedRoutes Routes

server :: Server Api
server = recordServer

recordServer :: Routes AsServer
recordServer =
  Routes
    { getInt = pure 7
    , captureVoid = const (pure ())
    }

badServer :: Server Api
badServer = badRecordServer

badRecordServer :: Routes AsServer
badRecordServer =
  Routes
    { getInt = throwError err500
    , captureVoid = const (pure ())
    }

data ComplexRoutes mode = ComplexRoutes
  { nested :: mode :- "nested" :> Capture "level" Int :> QueryParam "extra" Int :> Get '[JSON] Int
  , optional :: mode :- QueryParam "bonus" Int :> Header' '[Required] "flag" Int :> Get '[JSON] Int
  , combined :: mode :- "combine" :> Capture "first" Int :> Capture "second" Int :> ReqBody '[JSON] Int :> Put '[JSON] Int
  }
  deriving stock (Generic)

type ComplexApi = NamedRoutes ComplexRoutes

complexServer :: Server ComplexApi
complexServer = complexRecordServer

complexRecordServer :: ComplexRoutes AsServer
complexRecordServer =
  ComplexRoutes
    { nested = \lvl mExtra -> pure $ lvl + maybe 0 (* 2) mExtra
    , optional = \mBonus hdr -> pure $ maybe 0 id mBonus + hdr
    , combined = \a b body -> pure (a + b + body)
    }

complexBadServer :: Server ComplexApi
complexBadServer = complexBadRecordServer

complexBadRecordServer :: ComplexRoutes AsServer
complexBadRecordServer =
  ComplexRoutes
    { nested = \_ _ -> throwError err500
    , optional = \_ _ -> throwError err500
    , combined = \_ _ _ -> throwError err500
    }
