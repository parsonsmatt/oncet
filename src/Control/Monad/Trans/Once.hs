{-# language GADTs, ImportQualifiedPost, RecordWildCards, BlockArguments, DerivingStrategies, RankNTypes, GeneralizedNewtypeDeriving #-}

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
import Control.DeepSeq

-- | Perform a computation. Effects may be lifted into the type using the
-- 'declare' function, or the 'MonadTrans' or 'MonadIO' instances.
--
-- By default, effects that are lifted into this type *are not run* unless
-- the *result* of those actions are demanded. Additionally, they are run
-- *lazily* - when you actually demand the value.
newtype OnceT m a = OnceT { unOnceT :: ReaderT (UnliftIO m) IO a }
    deriving newtype (Functor, Applicative, Monad)

-- | This datatype provides the customization point for behavior of the
-- 'OnceT' type.
data OnceOpts m a = OnceOpts
    { onceOptsUnlifter :: UnliftIO m
    -- ^ The function for running your @m@ type in 'IO'. We use this
    -- instead of a concrete dependency on 'MonadUnliftIO' because not all
    -- interesting types *are* 'MonadUnliftIO', and this allows you to
    -- specify a behavior for those types.
    , onceOptsEvaluationStrategy :: OnceEvaluationStrategy a
    -- ^ The evaluation strategy for the effects in question. Your choice
    -- here will determine how much of the 'unsafeInterleaveIO' magic
    -- leaves the 'runOnceT' call.
    }

-- | The strategy for evaluating a 'OnceT' value, which determines how much
-- lazy IO your program is exposed to.
data OnceEvaluationStrategy a where
    -- | The safest and probably best choice. This ensures that the @a@
    -- value returned from the call is deeply evaluated with the
    -- 'evaluateDeep' function, which means that no effects from the recipe
    -- will happen after you exit the call to evaluate the recipe.
    --
    -- The only problem with this is that it will force a traversal of the
    -- data structure. If you have a huge list or record with a ton of
    -- fields, then this traversal may be time intensive.
    EvaluateDeeply :: NFData a => OnceEvaluationStrategy a
    -- | This evaluation strategy will evaluate the value to
    -- weak-head-normal-form - that is, the first constructor will be
    -- evaluated. This is a good choice if your type uses bang patterns or
    -- strict fields throughout, and you really want to avoid traversing
    -- the type unnecessarily.
    --
    -- This will call the effects that are necessary to produce this
    -- initial constructor, and any effects that those depend on. However,
    -- it will not call *all* of the effects - only those that are demanded
    -- lazily.
    UnsafeEvaluateShallow :: OnceEvaluationStrategy a
    -- | This is a pretty unsafe method! With this, the
    -- 'unsafeRunOnceTWith' call will terminate immediately, given you
    -- a completey lazy value. The effects for producing the value will be
    -- given completely lazily. This strategy exposes you to all the risk
    -- and fear of 'unsafeInterleaveIO'. Use at your own risk!
    UnsafeDoNotEvaluate :: OnceEvaluationStrategy a

-- | These are a set of safe options for running a 'OnceT'. We
safeOnceOpts :: NFData a => UnliftIO m -> OnceOpts m a
safeOnceOpts runInIO = OnceOpts runInIO EvaluateDeeply

-- | Evaluate an @a@ to WHNF.
evaluateShallow :: a -> IO a
evaluateShallow a = pure $! a

-- | The behavior of @'MonadTrans' 'OnceT'@ is that actions are lifted
-- *lazily*. To lift actions promptly, see 'promptlyRun', which runs the
-- action immediately.
instance MonadTrans OnceT where
    lift = declare

-- | The behavior of @'MonadIO' 'OnceT'@ is that actions are lifted
-- *lazily*. To lift 'IO' actions promptly, see 'promptlyRunIO', which runs
-- the action immediately.
instance  MonadIO (OnceT m) where
    liftIO = declareIO

-- | Run an action in 'OnceT' using the underyling type's 'MonadUnliftIO'
-- constraint. Any action that is 'declare'd but not demanded from
-- this action will not be run.
--
-- This function is a generally safe choice, because we call 'evaluateDeep'
-- on the result - so anything you actually return from the 'OnceT'
-- calculation will be evaluated, and all relevant effects will be run,
-- while we're still actually running this. Once you receive that @a@ out
-- of this function, you'll be done running effects.
--
-- For example, let's say you define a recipe for computing a bunch of
-- things together:
--
-- @
-- foo :: OnceT DB Int
-- bar :: OnceT DB Char
-- baz :: OnceT DB String
--
-- composite :: OnceT DB (Int, Char, String)
-- composite = do
--   a <- foo
--   b <- baz
--   c <- bar
--   pure (a, b, c)
-- @
--
-- But you only actually need the first item in the tuple. We can extract
-- the item from the tuple, and then it won't run the other effects.
--
-- @
-- main = do
--     result <- runOnceT do
--         (a, _, _) <- composite
--         pure a
--     print result
-- @
--
-- However, you may want to customize this behavior - you may desire
-- additional laziness. For that, see 'unsafRunOnceT'.
runOnceT :: (NFData a, MonadUnliftIO m) => OnceT m a -> m a
runOnceT action = do
    unlifter <- askUnliftIO
    unsafeRunOnceTWith (safeOnceOpts unlifter) action

-- | Run a 'OnceT' action and provide your own options. This function is
-- given the *unsafe* prefix because, depending on the options you select,
-- may or may not have weird lazy IO behavior.
unsafeRunOnceTWith :: MonadIO n => OnceOpts m a -> OnceT m a -> n a
unsafeRunOnceTWith OnceOpts {..} (OnceT action) = do
    liftIO do
        value <- runReaderT action onceOptsUnlifter
        case onceOptsEvaluationStrategy of
            EvaluateDeeply ->
                evaluateDeep value
            UnsafeEvaluateShallow ->
                pure $! value
            UnsafeDoNotEvaluate ->
                pure value


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

-- | Run an action immediately, without waiting for the result to be
-- evaluated.
promptlyRun :: m a -> OnceT m a
promptlyRun action = OnceT do ReaderT \(UnliftIO runInIO) -> runInIO action

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
