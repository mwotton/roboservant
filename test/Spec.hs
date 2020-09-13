{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

import           Roboservant
import           Servant.API
import           Test.Hspec
import qualified Roboservant.StateMachine as SM

newtype Foo = Foo Int
  deriving (Show)

newtype Bar = Bar String
  deriving (Show)

type FooApi = "foo" :> "fle" :> "far" :> Get '[JSON] Foo

type BarApi = "bar" :> ReqBody '[JSON] Foo :> Post '[JSON] Bar

type Api =
  FooApi :<|> BarApi

type Foo' = ExtractRespType FooApi

test :: ()
test = foo'EqualFoo
  where
    foo'EqualFoo :: Foo' ~ Foo => ()
    foo'EqualFoo = ()

test' :: ()
test' = blah
  where
    blah :: ExtractRespTypes Api ~ '[Foo, Bar] => ()
    blah = ()

storeOfOurApi = storeOfApi @Api

main :: IO ()
main = SM.tests  >>= print
