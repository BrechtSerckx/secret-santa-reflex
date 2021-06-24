{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

import           Prelude                 hiding ( catch
                                                , gets
                                                , put
                                                , runState
                                                , try
                                                )
import           Test.Hspec

import           Data.Functor.Compose
import           Data.IORef
import           Data.Maybe                     ( fromJust )
import           Polysemy
import           Polysemy.Error
import           Polysemy.Extra
import           Polysemy.Input
import           Polysemy.Operators
import           Polysemy.State

-- * Connection class

class Connection c where
  startTransaction :: c -> IO ~@> ()
  endTransaction :: c -> IO ~@> ()
  rollbackTransaction :: c -> IO ~@> ()

-- * Dummy Connections

newtype Conn = Conn { unConn :: IORef [Text] }

instance Connection Conn where
  startTransaction (Conn conn) = embed $ modifyIORef' conn (<> ["start"])
  endTransaction (Conn conn) = embed $ modifyIORef' conn (<> ["end"])
  rollbackTransaction (Conn conn) = embed $ modifyIORef' conn (<> ["rollback"])

-- ** Interpretations

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
 where
  runInputList' :: [i] -> Sem (Input i ': r) a -> Sem r a
  runInputList' is = fmap snd . runState (cycle is) . reinterpret
    (\case
      Input -> do
        s <- gets $ fromJust . uncons
        put $ snd s
        pure $ fst s
    )

-- * Errors

newtype MyError = MyError Text
  deriving stock (Eq, Show)
  deriving anyclass Exception

newtype MyError2 = MyError2 Text
  deriving stock (Eq, Show)
  deriving anyclass Exception

newtype F a = F (Either MyError2 (Either MyError a))
  deriving newtype (Eq, Show)

-- * Transactions

data Transaction c m a where
  Transact ::(c -> IO a) -> Transaction c m a
makeSem ''Transaction

-- ** Actions

insertSomething :: Text -> '[Transaction Conn] >@> ()
insertSomething msg = transact $ \(Conn conn) -> modifyIORef' conn (<> [msg])

-- ** Interpretations

runTransaction'
  :: Member (Embed IO) r => c -> Transaction c ': r @> a -> Input c ': r @> a
runTransaction' conn = reinterpret $ \case
  Transact f -> embed $ f conn

runTransaction
  :: forall c r a
   . (Member (Embed IO) r, Connection c)
  => Transaction c ': r @> a
  -> Input c ': r @> a
runTransaction act = do
  conn <- input @c
  startTransaction conn
  res <- runTransaction' conn act
  endTransaction conn
  pure res

runTransactionError
  :: forall c r a
   . (Members '[Error MyError , Embed IO , Final IO] r, Connection c)
  => Transaction c ': r @> a
  -> Input c ': r @> a
runTransactionError act = do
  conn <- input @c
  startTransaction conn
  res <-
    (fromExceptionSem @MyError $ runTransaction' conn act)
      `catch` \err@(MyError msg) -> do
                rollbackTransaction conn
                throw err
  endTransaction conn
  pure res

runTransactionErrors
  :: forall es f r c
   . ( Members '[Embed IO , Final IO] r
     , Members '[Embed IO , Final IO] (es :++ r)
     , Connection c
     )
  => ((Input c : (es :++ r)) @> () -> Input c ': r @> f ())
  -> (f () -> Bool)
  -> Transaction c ': (es :++ r) @> ()
  -> Input c ': r @> f ()
runTransactionErrors runErrors hasErrors act = do
  conn <- input
  startTransaction conn
  eRes <- runErrors . runTransaction' conn $ act
  if hasErrors eRes then rollbackTransaction conn else endTransaction conn
  pure eRes

type family Errors es where
  Errors '[] = '[]
  Errors (e ': es) = Error e ': Errors es

type family Eithers es a where
  Eithers '[] a = a
  Eithers (e ': es) a = Eithers es (Either e a)

class RunErrors es where
  runErrors :: Input c ': Errors es :++ r @> a -> Input c ': r @> Eithers es a
instance RunErrors '[] where
  runErrors
    :: Input c ': Errors '[] :++ r @> a -> Input c ': r @> Eithers '[] a
  runErrors = identity
instance RunErrors es => RunErrors ((e :: *) ': es) where
  runErrors
    :: forall r a c
     . Input c ': Errors (e ': es) :++ r @> a
    -> Input c ': r @> Eithers (e ': es) a
  runErrors = runErrors @es . runError @e . rotateEffects2

class HasErrors' es a where
  hasErrors' :: a -> Bool
instance HasErrors' '[] a where
  hasErrors' :: a -> Bool
  hasErrors' _ = False
instance HasErrors' es a => HasErrors' (e ': es) (Either e a) where
  hasErrors' :: Either e a -> Bool
  hasErrors' = \case
    Left  _ -> True
    Right a -> hasErrors' @es @a a

type HasErrors es a = HasErrors' (Reverse es) a
hasErrors :: forall es a . HasErrors es a => a -> Bool
hasErrors = hasErrors' @(Reverse es)

runTransactionErrors'
  :: forall es r c
   . ( Members '[Embed IO , Final IO] r
     , Members '[Embed IO , Final IO] (Errors es :++ r)
     , Connection c
     , RunErrors es
     , HasErrors es (Eithers es ())
     )
  => Transaction c ': (Errors es :++ r) @> ()
  -> Input c ': r @> (Eithers es ())
runTransactionErrors' act = do
  conn <- input
  startTransaction conn
  eRes <- runErrors @es . runTransaction' conn $ act
  if hasErrors @es eRes then rollbackTransaction conn else endTransaction conn
  pure eRes
-- * Misc

type family (:++) (as ::[k]) (bs ::[k]) :: [k] where
  '[] :++ bs = bs
  (a ': as) :++ bs = a ': (as :++ bs)
infixr 4 :++

type Reverse (xs :: [k]) = Reverse' xs '[]
type family Reverse' (xs::[k]) ( ys ::[k] ) :: [k] where
  Reverse' '[] ys = ys
  Reverse' (x ': xs) ys = Reverse' xs (x ': ys)

-- * Spec

wrapTransactionSpec
  :: (r ~ '[Embed IO], a ~ ())
  => (Input Conn ': r @> a -> r @> (c, a))
  -> (c -> IO ())
  -> SpecWith (Arg (IO ()))
wrapTransactionSpec runConn' runRes = it "wraps transactions altogether" $ do
  res <- liftIO . runM . runConn' . runTransaction $ do
    insertSomething "insert 1"
    insertSomething "insert 2"
  runRes $ fst res

separateConnTransactionSpec
  :: (r ~ '[Embed IO], a ~ ())
  => (Input Conn ': r @> a -> r @> (c, a))
  -> ((c, c) -> IO ())
  -> SpecWith (Arg (IO ()))
separateConnTransactionSpec runConn' runRes =
  it "separates transactions with different conns" $ do
    res1 <- liftIO . runM . runConn' . runTransaction $ insertSomething
      "insert 1"
    res2 <- liftIO . runM . runConn' . runTransaction $ insertSomething
      "insert 2"
    runRes (fst res1, fst res2)

separateTransactionSpec
  :: (r ~ '[Embed IO], a ~ ())
  => (Input Conn ': r @> a -> r @> (c, a))
  -> (c -> IO ())
  -> SpecWith (Arg (IO ()))
separateTransactionSpec runConn' runRes =
  it "separates transactions within same conn" $ do
    res <- liftIO . runM . runConn' $ do
      runTransaction $ insertSomething "insert 1"
      runTransaction $ insertSomething "insert 2"
    runRes $ fst res

rollbackErrorTransactionSpec
  :: (r ~ '[Embed IO, Final IO], a ~ Either MyError ())
  => (Input Conn ': r @> a -> r @> (c, a))
  -> ((c, a) -> IO ())
  -> SpecWith (Arg (IO ()))
rollbackErrorTransactionSpec runConn' runRes =
  it "rolls back transaction on error" $ do
    let act :: '[  Transaction Conn , Error MyError, Embed IO, Final IO ] @> ()
        act = do
          insertSomething "insert 1"
          throw $ MyError "error 1"
          insertSomething "insert 2"
    (res, eErr) :: (res, Either MyError ()) <-
      liftIO
      . runFinal
      . embedToFinal
      . runConn'
      . runError
      . rotateEffects2
      . runTransactionError
      $ act
    runRes $ (res, eErr)

rollbackExceptionTransactionSpec
  :: (r ~ '[Embed IO, Final IO], a ~ Either MyError ())
  => (Input Conn ': r @> a -> r @> (c, a))
  -> ((c, a) -> IO ())
  -> SpecWith (Arg (IO ()))
rollbackExceptionTransactionSpec runConn' runRes =
  it "rolls back transaction on exception" $ do
    let act :: '[  Transaction Conn , Error MyError, Embed IO, Final IO ] @> ()
        act = do
          insertSomething "insert 1"
          throwIO $ MyError "error 1"
          insertSomething "insert 2"
    (res, eErr) :: (res, Either MyError ()) <-
      liftIO
      . runFinal
      . embedToFinal
      . runConn'
      . runError
      . rotateEffects2
      . runTransactionError
      $ act
    runRes $ (res, eErr)

spec :: Spec
spec = describe "Polysemy" $ do

  -- * simple connection

  describe "runConn . runTransaction" $ do
    wrapTransactionSpec runConn
      $ \conn -> conn `shouldBe` ["start", "insert 1", "insert 2", "end"]

    separateConnTransactionSpec runConn $ \(c1, c2) -> do
      c1 `shouldBe` ["start", "insert 1", "end"]
      c2 `shouldBe` ["start", "insert 2", "end"]

    separateTransactionSpec runConn $ \c -> do
      c `shouldBe` ["start", "insert 1", "end", "start", "insert 2", "end"]

    rollbackErrorTransactionSpec runConn $ \(res, eErr) -> do
      res `shouldBe` ["start", "insert 1", "rollback"]
      eErr `shouldBe` Left (MyError "error 1")

    rollbackExceptionTransactionSpec runConn $ \(res, eErr) -> do
      res `shouldBe` ["start", "insert 1", "rollback"]
      eErr `shouldBe` Left (MyError "error 1")

  -- * connection pool

  describe "runConnPool . runTransaction" $ do
    wrapTransactionSpec (runConnPool 3) $ \res ->
      res `shouldBe` [["start", "insert 1", "insert 2", "end"], [], []]

    separateConnTransactionSpec (runConnPool 3) $ \(c1, c2) -> do
      c1 `shouldBe` [["start", "insert 1", "end"], [], []]
      c2 `shouldBe` [["start", "insert 2", "end"], [], []]

    separateTransactionSpec (runConnPool 3) $ \c -> do
      c
        `shouldBe` [ ["start", "insert 1", "end"]
                   , ["start", "insert 2", "end"]
                   , []
                   ]

    rollbackErrorTransactionSpec (runConnPool 3) $ \(res, eErr) -> do
      res `shouldBe` [["start", "insert 1", "rollback"], [], []]
      eErr `shouldBe` Left (MyError "error 1")

    rollbackExceptionTransactionSpec (runConnPool 3) $ \(res, eErr) -> do
      res `shouldBe` [["start", "insert 1", "rollback"], [], []]
      eErr `shouldBe` Left (MyError "error 1")

  -- * multiple errors

  describe "multiple errors" $ do

    it "rolls back transaction on errors" $ do
      let
        act
          :: '[  Transaction Conn , Error MyError, Error MyError2, Embed IO, Final IO ] @> ()
        act = do
          insertSomething "insert 1"
          throw $ MyError "error 1"
          insertSomething "insert 2"
          throw $ MyError2 "error 2"
          insertSomething "insert 3"

        runErrors
          :: (  (Input c : ('[Error MyError, Error MyError2] :++ r)) @> ()
             -> Input c ': r @> F ()
             )
        runErrors = fmap F . runError . runError . rotateEffects3L
        hasErrors :: F () -> Bool
        hasErrors = \case
          F (Right (Right _)) -> False
          _                   -> True

      (res, eErr) :: ([Text], F ()) <-
        liftIO
        . runFinal
        . embedToFinal
        . runConn
        . runTransactionErrors @'[Error MyError , Error MyError2] runErrors
                                                                  hasErrors
        $ act
      res `shouldBe` ["start", "insert 1", "rollback"]
      eErr `shouldBe` F (Right (Left (MyError "error 1")))

    it "rolls back transaction on errors 2" $ do
      let
        act
          :: '[  Transaction Conn, Error MyError, Error MyError2, Embed IO, Final IO ] @> ()
        act = do
          insertSomething "insert 1"
          insertSomething "insert 2"
          throw $ MyError2 "error 2"
          insertSomething "insert 3"

        runErrors
          :: (  (Input Conn : ('[Error MyError, Error MyError2] :++ r)) @> ()
             -> Input Conn ': r @> F ()
             )
        runErrors = fmap F . runError . runError . rotateEffects3L
        hasErrors :: F () -> Bool
        hasErrors = \case
          F (Right (Right _)) -> False
          _                   -> True

      (res, eErr) :: ([Text], F ()) <-
        liftIO
        . runFinal
        . embedToFinal
        . runConn
        . runTransactionErrors @'[Error MyError , Error MyError2] runErrors
                                                                  hasErrors
        $ act
      res `shouldBe` ["start", "insert 1", "insert 2", "rollback"]
      eErr `shouldBe` F (Left (MyError2 "error 2"))

  -- * multiple errors but better

  describe "multiple errors but better" $ do


    it "rolls back transaction on errors" $ do
      let
        act
          :: '[  Transaction Conn , Error MyError, Error MyError2, Embed IO, Final IO ] @> ()
        act = do
          insertSomething "insert 1"
          throw $ MyError "error 1"
          insertSomething "insert 2"
          throw $ MyError2 "error 2"
          insertSomething "insert 3"

      (res, eErr) <-
        liftIO
        . runFinal
        . embedToFinal
        . runConn
        . runTransactionErrors' @'[MyError , MyError2]
        $ act
      res `shouldBe` ["start", "insert 1", "rollback"]
      eErr `shouldBe` Right (Left (MyError "error 1"))

    it "rolls back transaction on errors 2" $ do
      let
        act
          :: '[  Transaction Conn, Error MyError, Error MyError2, Embed IO, Final IO ] @> ()
        act = do
          insertSomething "insert 1"
          insertSomething "insert 2"
          throw $ MyError2 "error 2"
          insertSomething "insert 3"

      (res, eErr) <-
        liftIO
        . runFinal
        . embedToFinal
        . runConn
        . runTransactionErrors' @'[MyError , MyError2]
        $ act
      res `shouldBe` ["start", "insert 1", "insert 2", "rollback"]
      eErr `shouldBe` Left (MyError2 "error 2")

main :: IO ()
main = hspec spec


