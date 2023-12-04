{-# language BlockArguments #-}
{-# language LambdaCase #-}

import Data.List (partition)
import Test.Hspec
import Control.Monad.Trans.Once
import Control.Concurrent.STM (check)
import Data.Traversable
import Control.Monad
import Data.Foldable
import Control.Monad.IO.Class
import UnliftIO

main :: IO ()
main = hspec spec

newtype OnceVar a = OnceVar (MVar a)

putOnceVar :: HasCallStack => OnceVar a -> a -> IO ()
putOnceVar (OnceVar var) a = do
    written <- tryPutMVar var a
    if written
        then pure ()
        else error "Tried to write already written MVar"

newOnceVar :: IO (OnceVar a)
newOnceVar = OnceVar <$> newEmptyMVar

readOnceVar :: OnceVar a -> IO (Maybe a)
readOnceVar (OnceVar a) = tryReadMVar a

shouldBeUnwritten :: HasCallStack => (Show a, Eq a) => OnceVar a -> IO ()
shouldBeUnwritten a = readOnceVar a `shouldReturn` Nothing

shouldBeWritten :: HasCallStack => (Show a, Eq a) => OnceVar a -> IO ()
shouldBeWritten a = do
    mval <- readOnceVar a
    mval `shouldSatisfy` \case
        Just _ -> True
        Nothing -> False

unsafeRunOnceT :: OnceT IO a -> IO a
unsafeRunOnceT = unsafeRunOnceTWith (OnceOpts (UnliftIO id) UnsafeDoNotEvaluate)

spec :: Spec
spec = describe "OnceT" do
    it "actions are run at most once" do
        a <- newOnceVar
        let firstAction = do
                putOnceVar a ()
                pure "hello"
        result <-
            unsafeRunOnceT do
                declare firstAction
        readOnceVar a `shouldReturn` Nothing
        result `shouldBe` "hello"
        result `shouldBe` "hello"
        readOnceVar a `shouldReturn` Just ()

    it "only runs actions if the result is demanded" do
        unwritten <- newOnceVar
        written <- newOnceVar

        result <-
            unsafeRunOnceT do
                a <- declare do
                    putOnceVar unwritten ()
                    pure "hello"
                b <- declare do
                    putOnceVar written ()
                    pure "goodbye"
                pure (a, b)

        readOnceVar unwritten `shouldReturn` Nothing
        readOnceVar written `shouldReturn` Nothing
        snd result `shouldBe` "goodbye"
        readOnceVar unwritten `shouldReturn` Nothing
        readOnceVar written `shouldReturn` Just ()
        fst result `shouldBe` "hello"
        readOnceVar unwritten `shouldReturn` Just ()
        readOnceVar written `shouldReturn` Just ()

    it "works on lists" do
        onceVars <- replicateM 10 newOnceVar
        let indexedOnceVars = zip [0..] onceVars
        result <-
            unsafeRunOnceT do
                for indexedOnceVars \(i, var) -> do
                    declare do
                        putOnceVar var ()
                        pure i

        for_ onceVars \onceVar -> do
            readOnceVar onceVar `shouldReturn` Nothing

        -- inspecting the spine does not force evaluation
        length result `shouldBe` 10
        for_ onceVars \onceVar -> do
            readOnceVar onceVar `shouldReturn` Nothing

        let evensAndOdds xs =
                let (odds, evens) = partition (\(i, _) -> i `mod` 2 == 0) . zip [0..] $ xs
                    unindex = fmap snd
                 in (unindex odds, unindex evens)
            evens =
                fst . evensAndOdds

        -- inspecting half of the results
        evens result `shouldBe` [0, 2, 4, 6, 8]

        let (evenVars, oddVars) = evensAndOdds onceVars

        -- ... should force the evens
        for_ evenVars \onceVar -> do
            readOnceVar onceVar `shouldReturn` Just ()

        -- ... and not the odds
        for_ oddVars \onceVar -> do
            readOnceVar onceVar `shouldReturn` Nothing

    describe "concurrent access" do
        it "does not run twice" do
            var <- newOnceVar
            result <-
                unsafeRunOnceT do
                    declare do
                        putOnceVar var ()
                        pure (slowFib 35)
            goVar <- newTVarIO False

            let competeForResult =
                    atomically do
                        check =<< readTVar goVar
                        pure $ show result

            withAsync competeForResult \firstThread -> withAsync competeForResult \secondThread -> do
                -- they should read the result around the same time
                -- and computing slowFib of 35 takes a while anyway!
                atomically $ writeTVar goVar True

                len1 <- wait firstThread
                len2 <- wait secondThread
                result `shouldBe` 9227465
                len1 `shouldBe` "9227465"
                len2 `shouldBe` "9227465"

    describe "MonadIO behavior" do
        it "does not perform side effects if result is not demanded" do
            willBeWritten <- newOnceVar
            won'tBeWritten <- newOnceVar
            result <-
                unsafeRunOnceT do
                    a <- declare do
                        putOnceVar willBeWritten ()
                        pure 10

                    liftIO do
                        putOnceVar won'tBeWritten ()

                    pure a

            shouldBeUnwritten willBeWritten
            shouldBeUnwritten won'tBeWritten

            result `shouldBe` 10

            shouldBeWritten willBeWritten
            shouldBeUnwritten won'tBeWritten

slowFib :: Int -> Int
slowFib 0 = 0
slowFib 1 = 1
slowFib x = slowFib (x - 1) + slowFib (x - 2)
