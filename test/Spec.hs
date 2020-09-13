{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Data.Proxy (Proxy (..))
import qualified Foo
import Hedgehog (Group (..), checkSequential)
import qualified Roboservant as RS
import Servant (Endpoints)
import qualified UnsafeIO

-- | this is pretty bad. hopefully Jacob knows a better way of doing this.
--   https://twitter.com/mwotton/status/1305189249646460933
assert :: String -> Bool -> IO ()
assert _ True = pure ()
assert err False = ioError $ userError err

main :: IO ()
main = do
  let reifiedApi = RS.toReifiedApi (RS.flattenServer @Foo.FooApi Foo.fooServer) (Proxy @(Endpoints Foo.FooApi))
  assert "should find an error in Foo" . not
    =<< checkSequential (Group "Foo" [("Foo", RS.prop_sequential reifiedApi)])
  unsafeServer <- UnsafeIO.makeServer
  let unsafeApi = RS.toReifiedApi (RS.flattenServer @UnsafeIO.UnsafeApi unsafeServer) (Proxy @(Endpoints UnsafeIO.UnsafeApi))
  -- this will not detect the error, as it requires concurrency.
  assert "should find nothing" =<< checkSequential (Group "Unsafe" [("Sequential", RS.prop_sequential unsafeApi)])
  assert "should find with parallel check" . not
    =<< checkSequential (Group "Unsafe" [("Parallel", RS.prop_concurrent unsafeApi)])
