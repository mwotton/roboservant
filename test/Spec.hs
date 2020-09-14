{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import qualified Foo
import qualified Headers
import Hedgehog (Group (..), checkSequential, withTests)
import qualified Roboservant as RS
import qualified UnsafeIO

-- | this is pretty bad. hopefully Jacob knows a better way of doing this.
--   https://twitter.com/mwotton/status/1305189249646460933
assert :: String -> Bool -> IO ()
assert _ True = pure ()
assert err False = ioError $ userError err

-- | This is horribly laid out, sorry. Will fix at some point.
main :: IO ()
main = do
  assert "should find an error in Foo" . not
    =<< checkSequential (Group "Foo" [("Foo", withTests 100000 $ RS.prop_sequential @Foo.FooApi Foo.fooServer)])

  assert "should find an error in Headers" . not
    =<< checkSequential (Group "Headers" [("Headers", withTests 10000 $ RS.prop_sequential @Headers.Api Headers.server)])
    
  -- The UnsafeIO checker does not actually really use the contextually aware stuff, though it
  -- could: it's mostly here to show how to test for concurrency problems.
  unsafeServer <- UnsafeIO.makeServer
  -- this will not detect the error, as it requires concurrency.
  assert "should find nothing" =<< checkSequential (Group "Unsafe" [("Sequential", RS.prop_sequential @UnsafeIO.UnsafeApi unsafeServer)])
  -- this will!
  assert "should find with parallel check" . not
    =<< checkSequential (Group "Unsafe" [("Parallel", withTests 100000 $ RS.prop_concurrent @UnsafeIO.UnsafeApi unsafeServer)])
