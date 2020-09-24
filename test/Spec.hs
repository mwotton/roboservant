{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import qualified Foo
import qualified Headers
import Hedgehog (Group (..), checkSequential, withTests)
import qualified Roboservant as RS
import qualified UnsafeIO
import Control.Monad(when)
import Test.Hspec.Hedgehog
import Test.Hspec
import Test.Hspec.Core.Spec(shouldFail)
import Control.Monad.Trans (MonadIO(liftIO))

-- -- | this is pretty bad. hopefully Jacob knows a better way of doing this.
-- --   https://twitter.com/mwotton/status/1305189249646460933
-- assert :: String -> Bool -> IO ()
-- assert _ True = pure ()
-- assert err False = ioError $ userError err

main = hspec spec

spec :: Spec
spec = do
  describe "Foo" $ do
    --
    shouldFail $ it "finds an error" $
      (hedgehog $ RS.prop_sequential @Foo.FooApi Foo.fooServer [])

  describe "Headers" $ do
    shouldFail $ it "should find a failure that's dependent on using header info" $ do
      liftIO $ pendingWith "doesn't yet work"
      (hedgehog $ RS.prop_sequential @Headers.Api Headers.server [])

  -- -- The UnsafeIO checker does not actually really use the contextually aware stuff, though it
  -- -- could: it's mostly here to show how to test for concurrency problems.
  before UnsafeIO.makeServer $ do
    describe "sequential checking" $ do
      it "safe use" $ \unsafeServer -> do
        hedgehog $ RS.prop_sequential @UnsafeIO.UnsafeApi unsafeServer []

    modifyMaxSuccess (const 10000) $
      shouldFail $
        describe "concurrent" $ do
          it "concurrent, dangerous use" $ \unsafeServer -> do
            RS.prop_concurrent @UnsafeIO.UnsafeApi unsafeServer
