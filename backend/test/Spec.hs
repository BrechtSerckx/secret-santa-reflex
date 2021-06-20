{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

import           Prelude                 hiding ( catch
                                                , gets
                                                , put
                                                , runState
                                                , try
                                                )
import           Test.Hspec

import           Data.IORef
import           Data.Maybe                     ( fromJust )
import           Polysemy
import           Polysemy.Error
import           Polysemy.Extra
import           Polysemy.Input
import           Polysemy.Operators
import           Polysemy.State

newtype Conn = Conn { unConn :: IORef [Text] }
data Transaction m a where
  Transact ::(Conn -> IO a) -> Transaction m a
makeSem ''Transaction

insertSomething :: Text -> '[Transaction] >@> ()
insertSomething msg = transact $ \(Conn conn) -> modifyIORef' conn (<> [msg])

startTransaction :: Conn -> IO ~@> ()
startTransaction (Conn conn) = embed $ modifyIORef' conn (<> ["start"])

endTransaction :: Conn -> IO ~@> ()
endTransaction (Conn conn) = embed $ modifyIORef' conn (<> ["end"])

rollbackTransaction :: Conn -> IO ~@> ()
rollbackTransaction (Conn conn) = embed $ modifyIORef' conn (<> ["rollback"])

runConn :: Input Conn ': r @> a -> IO ~@ r @> ([Text], a)
runConn act = do
  conn <- embed $ newIORef []
  a    <- runInputConst (Conn conn) act
  s    <- embed $ readIORef conn
  pure (s, a)

runConnPool :: Int -> Input Conn ': r @> a -> IO ~@ r @> ([[Text]], a)
runConnPool n act = do
  cs :: [IORef [Text]] <- embed . replicateM n $ newIORef []
  let conns = Conn <$> cs
  a <- runInputList' conns act
  s <- embed $ traverse readIORef cs
  pure (s, a)

newtype MyError = MyError Text
  deriving stock (Eq, Show)
  deriving anyclass Exception

newtype MyError2 = MyError2 Text
  deriving stock (Eq, Show)
  deriving anyclass Exception

runTransaction'
  :: Member (Embed IO) r
  => Conn
  -> Transaction ': r @> a
  -> Input Conn ': r @> a
runTransaction' conn = reinterpret $ \case
  Transact f -> embed $ f conn

runTransaction
  :: Member (Embed IO) r => Transaction ': r @> a -> Input Conn ': r @> a
runTransaction act = do
  conn <- input
  startTransaction conn
  res <- runTransaction' conn act
  endTransaction conn
  pure res

runTransactionError
  :: Members '[Error MyError , Embed IO , Final IO] r
  => Transaction ': r @> a
  -> Input Conn ': r @> a
runTransactionError act = do
  conn <- input
  startTransaction conn
  res <-
    (fromExceptionSem @MyError $ runTransaction' conn act)
      `catch` \err@(MyError msg) -> do
                rollbackTransaction conn
                throw err
  endTransaction conn
  pure res

-- runTransactionErrors
--   :: forall es r
--    . Members '[Embed IO , Final IO] r
--   => Conn
--   -> Transaction ': (es :++ r) @> ()
--   -> r @> (Conn, Either MyError ())
-- runTransactionErrors conn act = atomicStateToIO conn $ do
--   embed $ startTransaction conn
--   let act' :: AtomicState Conn ': (es :++ r) @> ()
--       act' = runTransactionState act
--       act'' :: (AtomicState Conn ': r) @> Either MyError ()
--       act'' = runErrors @es act'
--   eRes <- act''
--   case eRes of
--     Right ()  -> pure ()
--     Left  err -> embed $ rollbackTransaction conn
--   embed $ endTransaction conn
--   pure eRes

-- runErrors
--   :: forall es r a
--    . Members '[Embed IO , Final IO] r
--   => AtomicState Conn ': (es :++ r) @> ()
--   -> (AtomicState Conn ': r) @> Either MyError ()
-- runErrors = undefined

type family (:++) as bs where
  '[] :++ bs = bs
  (a ': as) :++ bs = a ': (as :++ bs)
infixr 4 :++

spec :: Spec
spec = describe "Polysemy" $ do
  describe "runConn . runTransaction" $ do
    it "wraps transactions altogether" $ do
      res <- liftIO . runM . runConn . runTransaction $ do
        insertSomething "insert 1"
        insertSomething "insert 2"
      fst res `shouldBe` ["start", "insert 1", "insert 2", "end"]

    it "separates transactions" $ do
      res1 <- liftIO . runM . runConn . runTransaction $ insertSomething
        "insert 1"
      res2 <- liftIO . runM . runConn . runTransaction $ insertSomething
        "insert 2"
      fst res1 `shouldBe` ["start", "insert 1", "end"]
      fst res2 `shouldBe` ["start", "insert 2", "end"]

    it "rolls back transaction on error" $ do
      let act :: '[  Transaction , Error MyError, Embed IO, Final IO ] @> ()
          act = do
            insertSomething "insert 1"
            throw $ MyError "error 1"
            insertSomething "insert 2"
      (res, eErr) <-
        liftIO
        . runFinal
        . embedToFinal
        . runConn
        . runError
        . rotateEffects2
        . runTransactionError
        $ act
      res `shouldBe` ["start", "insert 1", "rollback"]
      eErr `shouldBe` Left (MyError "error 1")

  describe "runConnPool . runTransaction" $ do
    it "wraps transactions altogether" $ do
      res <- liftIO . runM . runConnPool 3 . runTransaction $ do
        insertSomething "insert 1"
        insertSomething "insert 2"
      fst res `shouldBe` [["start", "insert 1", "insert 2", "end"], [], []]

    it "separates transactions with different pools" $ do
      res1 <- liftIO . runM . runConnPool 3 . runTransaction $ insertSomething
        "insert 1"
      res2 <- liftIO . runM . runConnPool 3 . runTransaction $ insertSomething
        "insert 2"
      fst res1 `shouldBe` [["start", "insert 1", "end"], [], []]
      fst res2 `shouldBe` [["start", "insert 2", "end"], [], []]

    it "separates transactions within a pool" $ do
      res <- liftIO . runM . runConnPool 3 $ do
        runTransaction $ insertSomething "insert 1"
        runTransaction $ insertSomething "insert 2"
      fst res
        `shouldBe` [ ["start", "insert 1", "end"]
                   , ["start", "insert 2", "end"]
                   , []
                   ]

    it "rolls back transaction on error" $ do
      let act :: '[  Transaction , Error MyError, Embed IO, Final IO ] @> ()
          act = do
            insertSomething "insert 1"
            throw $ MyError "error 1"
            insertSomething "insert 2"
      (res, eErr) <-
        liftIO
        . runFinal
        . embedToFinal
        . runConnPool 3
        . runError
        . rotateEffects2
        . runTransactionError
        $ act
      res `shouldBe` [["start", "insert 1", "rollback"], [], []]
      eErr `shouldBe` Left (MyError "error 1")

    it "rolls back transaction on exception" $ do
      let act :: '[  Transaction , Error MyError, Embed IO, Final IO ] @> ()
          act = do
            insertSomething "insert 1"
            throwIO $ MyError "error 1"
            insertSomething "insert 2"
      (res, eErr) <-
        liftIO
        . runFinal
        . embedToFinal
        . runConnPool 3
        . runError
        . rotateEffects2
        . runTransactionError
        $ act
      res `shouldBe` [["start", "insert 1", "rollback"], [], []]
      eErr `shouldBe` Left (MyError "error 1")

  -- it "rolls back transaction on errors" $ do
  --   conn <- liftIO $ newIORef []
  --   let
  --     act
  --       :: '[  Error MyError, Error MyError2, Transaction , Embed IO, Final IO ] @> ()
  --     act = do
  --       insertSomething "insert 1"
  --       throw $ MyError "error 1"
  --       insertSomething "insert 2"
  --       throw $ MyError2 "error 2"
  --       insertSomething "insert 3"
  --     act' :: '[  Embed IO, Final IO ] @> (Conn, Either MyError ())
  --     act' = runTransactionErrors @'[Error MyError, Error MyError2] conn act
  --     act'' :: '[  Final IO ] @> (Conn, Either MyError ())
  --     act'' = embedToFinal act'
  --     act''' :: IO (Conn, Either MyError ())
  --     act''' = runFinal act''
  --   eErr' :: (Conn, Either MyError ()) <- liftIO act'''
  --   let eErr :: Either MyError () = snd eErr'
  --   res <- liftIO $ readIORef conn
  --   res `shouldBe` ["start", "insert 1", "rollback"]
  --   eErr `shouldBe` Left (MyError "error 1")

main :: IO ()
main = hspec spec


runInputList' :: [i] -> Sem (Input i ': r) a -> Sem r a
runInputList' is = fmap snd . runState (cycle is) . reinterpret
  (\case
    Input -> do
      s <- gets $ fromJust . uncons
      put $ snd s
      pure $ fst s
  )
