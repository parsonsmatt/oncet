{-# language ImportQualifiedPost, BlockArguments, DerivingStrategies, RankNTypes, GeneralizedNewtypeDeriving #-}

-- | This module defines a datatype 'OnceT' that allows you to perform
-- actions *at most once* using the horribly dark cursed magical power of
-- lazy 'IO'.
--
-- The motivation of this library is to declare a record and produce it
-- effectfully, but only run the effects when the fields of the record are
-- demanded - and only once.
--
-- This is inherently dangerous, for all the reasons that unrestricted 'IO'
-- in a lazy language is dangerous. If you use this type, you must make
-- absolutely certain that the underlying actions are *safe* to perform at
-- any time.
--
-- For example, consider this potential use:
--
-- @
-- result <-
--      withFile "file.txt" ReadMode \\handle -> do
--          runOnce do
--              liftOnce do
--                  hGetContents handle
-- print $ length result
-- @
--
-- The action to get the file contents out of the @handle@ is *defined*
-- inside of the @withFile@ call, but the actual evaluation of the @result@
-- happens outside - and so the file handle may be closed, and this may
-- fail at runtime.
--
-- This library leverages the 'once' function from the @extra@ package.
module Control.Monad.Trans.Once where

import Control.Concurrent.Extra
import UnliftIO
import System.IO.Unsafe
import Control.Monad
import Control.Monad.Reader

-- | Perform a computation. Effects may be lifted into the type using the
-- 'declare' function.
newtype OnceT m a = OnceT { unOnceT :: ReaderT (UnliftIO m) IO a }
    deriving newtype (Functor, Applicative, Monad)

instance MonadTrans OnceT where
    lift = declare

instance  MonadIO (OnceT m) where
    liftIO = declareIO

-- | Run an action in 'OnceT' using the underyling type's 'MonadUnliftIO'
-- constraint. Any action that is 'declare'd but not demanded from
-- this action will not be run.
runOnceT :: MonadUnliftIO m => OnceT m a -> m a
runOnceT action = do
    UnliftIO unlifter <- askUnliftIO
    runOnceTWith unlifter action

-- | Run a 'OnceT' action and provide your own runner instead of relying on
-- 'MonadUnliftIO'. This allows you to use this library with types that
-- don't implement this.
--
-- Note that the function for unlifting actions will be called lazily!
runOnceTWith :: MonadIO n => (forall x. m x -> IO x) -> OnceT m a -> n a
runOnceTWith runner (OnceT action) = do
    liftIO $ runReaderT action (UnliftIO runner)

-- | Lift an action into the 'OnceT' transformer. The action is only
-- performed when the result of the action is demanded.
declare :: m a -> OnceT m a
declare action = do
    UnliftIO runInIO <- OnceT ask
    declareIO $ runInIO action

-- | Run an action immediately, without waiting for the result to be
-- evaluated.
promptlyRunIO :: IO a -> OnceT m a
promptlyRunIO action = OnceT do lift action

-- | Lift an 'IO' action into a 'OnceT' transformer. This does not require
-- a 'MonadIO' constraint, because the true base monad of 'OnceT' is 'IO'.
declareIO :: IO a -> OnceT m a
declareIO action = OnceT do
    ReaderT \_ -> do
        join $ once $ unsafeInterleaveIO action

example :: OnceT IO (Int, String)
example = do
    hello <- declare $ do
        putStrLn "declaring hello"
        pure "hello"
    num <- declare $ do
        putStrLn "declaring num"
        pure 20
    pure (num, hello)

fstOnce :: OnceT IO Int
fstOnce = fst <$> example

cool :: OnceT IO String
cool = do
    (num, hello) <- example
    if num >= 30
        then pure hello
        else pure (show num)
