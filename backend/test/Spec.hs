{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

import           Prelude                 hiding ( try
                                                , throw
                                                , catch
                                                )
import           Test.Hspec

import           Data.IORef
import           Polysemy
import           Polysemy.Error
import           Polysemy.AtomicState
import           Polysemy.Operators

type DBRef = IORef [Text]
data Transaction m a where
  Transact ::(DBRef -> IO a) -> Transaction m a
makeSem ''Transaction

newtype MyError = MyError Text
  deriving stock (Eq, Show)
  deriving anyclass Exception

insertSomething :: Text -> '[Transaction] >@> ()
insertSomething msg = transact $ \dbRef -> modifyIORef' dbRef (<> [msg])

runTransactionState
  :: Transaction ': r @> a -> IO ~@ AtomicState DBRef ': r @> a
runTransactionState = reinterpret $ \case
  Transact act -> do
    dbRef <- atomicGet
    embed $ act dbRef

runTransaction
  :: DBRef -> Transaction ': r @> a -> '[Embed IO] >@ r @> (DBRef, ())
runTransaction dbRef act = atomicStateToIO dbRef $ do
  embed $ modifyIORef' dbRef (<> ["start"])
  runTransactionState act
  embed $ modifyIORef' dbRef (<> ["end"])

runTransactionError
  :: DBRef
  -> Transaction ': r @> a
  -> '[Error MyError, Embed IO, Final IO] >@ r @> (DBRef, ())
runTransactionError dbRef act = atomicStateToIO dbRef $ do
  embed $ modifyIORef' dbRef (<> ["start"])
  (fromExceptionSem @MyError $ runTransactionState act)
    `catch` \err@(MyError msg) -> do
              embed $ modifyIORef' dbRef (<> ["rollback"])
              throw err
  embed $ modifyIORef' dbRef (<> ["end"])

spec :: Spec
spec = describe "Polysemy" $ do
  it "wraps transactions altogether" $ do
    dbRef <- liftIO $ newIORef []
    liftIO . runM . runTransaction dbRef $ do
      insertSomething "insert 1"
      insertSomething "insert 2"
    res <- liftIO $ readIORef dbRef
    res `shouldBe` ["start", "insert 1", "insert 2", "end"]

  it "separates transactions" $ do
    dbRef <- liftIO $ newIORef []
    liftIO . runM . runTransaction dbRef $ insertSomething "insert 1"
    liftIO . runM . runTransaction dbRef $ insertSomething "insert 2"
    res <- liftIO $ readIORef dbRef
    res `shouldBe` ["start", "insert 1", "end", "start", "insert 2", "end"]

  it "rolls back transaction on error" $ do
    dbRef <- liftIO $ newIORef []
    let act :: '[  Transaction , Error MyError, Embed IO, Final IO ] @> ()
        act = do
          insertSomething "insert 1"
          throw $ MyError "error 1"
          insertSomething "insert 2"
    eErr' <-
      liftIO
      . runFinal
      . embedToFinal
      . runError
      . runTransactionError dbRef
      $ act
    let eErr :: Either MyError () = second snd eErr'
    res <- liftIO $ readIORef dbRef
    res `shouldBe` ["start", "insert 1", "rollback"]
    eErr `shouldBe` Left (MyError "error 1")

  it "rolls back transaction on exception" $ do
    dbRef <- liftIO $ newIORef []
    let act :: '[  Transaction , Error MyError, Embed IO, Final IO ] @> ()
        act = do
          insertSomething "insert 1"
          throwIO $ MyError "error 1"
          insertSomething "insert 2"
    eErr' <-
      liftIO
      . runFinal
      . embedToFinal
      . runError
      . runTransactionError dbRef
      $ act
    let eErr :: Either MyError () = second snd eErr'
    res <- liftIO $ readIORef dbRef
    res `shouldBe` ["start", "insert 1", "rollback"]
    eErr `shouldBe` Left (MyError "error 1")

main :: IO ()
main = hspec spec
