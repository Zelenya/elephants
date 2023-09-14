{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Elephants.PersistentEsqueleto (runThis) where

import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, SomeException (..), catch, displayException, handle)
import Control.Monad.IO.Unlift (MonadIO (..))
import Control.Monad.Logger
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.Esqueleto.Experimental
import Database.Persist.Postgresql qualified as P
import Database.Persist.TH (mkPersist, persistLowerCase, sqlSettings)
import Hardcoded qualified

mkPersist
  sqlSettings
  [persistLowerCase|
  Product
    label Text
    description Text Maybe
    UniqueLabel label
    deriving Eq Show
  Category
    label Text
    UniqueCategory label
    deriving Eq Show
  ProductCategory
    productId ProductId
    categoryId CategoryId
    Primary productId categoryId
    deriving Eq Show
  Warehouse
    productId ProductId
    quantity Int
    created UTCTime default=CURRENT_TIME
    modified UTCTime default=CURRENT_TIME
    deriving Eq Show
|]

runThis :: IO ()
runThis = do
  putStrLn "\nRunning persistent/esqueleto"
  -- Alternative for debugging:
  -- runStdoutLoggingT $
  runNoLoggingT $ P.withPostgresqlPool Hardcoded.connectionString 3 $ \pool -> do
    let runWithPool = flip liftSqlPersistMPool pool
    runWithPool cleanUp
    runWithPool insertStuff
    runWithPool queryData
    runWithPool insertWithTransaction
    runWithPool queryWithJoins
    runWithPool errors
  putStrLn "Done"

cleanUp :: (MonadIO m) => SqlPersistT m ()
cleanUp = rawExecute "truncate warehouse, product_category, product, category" []

insertStuff :: (MonadIO m) => SqlPersistT m ()
insertStuff = do
  newId <- insert $ Product "Wood Screw Kit 1" (Just "245-pieces")
  liftIO $ putStrLn $ "Insert 1: " <> show newId

  newIds <- insertMany [Category "Screws", Category "Wood Screws", Category "Concrete Screws"]
  liftIO $ putStrLn $ "Insert 2: " <> show newIds

queryData :: forall m. (MonadIO m) => SqlPersistT m ()
queryData = do
  result1 <- query1 "Wood Screw Kit 1"
  liftIO $ putStrLn $ "Query 1: " <> show result1

  result2 <- query2 ["Wood Screw Kit 1", "Wood Screw Kit 2"]
  liftIO $ putStrLn $ "Query 2: " <> show result2
 where
  query1 :: Text -> SqlPersistT m [Entity Product]
  query1 label = select $ do
    aProduct <- from $ table @Product
    where_ (aProduct.label ==. val label)
    pure aProduct

  query2 :: [Text] -> SqlPersistT m [Entity Product]
  query2 lables = select $ do
    aProduct <- from $ table @Product
    where_ $ aProduct.label `in_` valList lables
    pure aProduct

insertWithTransaction :: (MonadIO m, MonadCatch m) => SqlPersistT m ()
insertWithTransaction = handle (\(SomeException _) -> pure ()) $ do
  productId <- insert $ Product "Drywall Screws Set" (Just "8000pcs")
  categoryId <- insert $ Category "Drywall Screws"
  time <- liftIO getCurrentTime
  _ <- insert_ $ Warehouse productId 10 time time
  _ <- insert_ $ ProductCategory productId categoryId
  liftIO $ putStrLn $ "Insert with transaction"

queryWithJoins :: forall m. (MonadIO m) => SqlPersistT m ()
queryWithJoins = do
  result <- query 3
  liftIO $ putStrLn $ "Query with join: " <> show result
 where
  query quantity = select $ do
    (warehouse :& aProduct :& _ :& category) <-
      from
        $ table @Warehouse
        `innerJoin` table @Product
        `on` do \(w :& p) -> w.productId ==. p.id
        `LeftOuterJoin` table @ProductCategory
        `on` do \(_ :& p :& pc) -> just p.id ==. pc.productId
        `LeftOuterJoin` table @Category
        `on` do \(_ :& _ :& pc :& c) -> pc.categoryId ==. c.id
    where_ (warehouse.quantity >. val quantity)
    pure $ (warehouse.quantity, aProduct.label, aProduct.description, category.label)

errors :: (MonadIO m, MonadCatch m) => SqlPersistT m ()
errors = do
  let duplicateScrew = Product "Duplicate screw" Nothing
  void $ insert duplicateScrew
  (void $ insert duplicateScrew)
    `catch` (\(SomeException err) -> liftIO $ putStrLn $ "Caught SQL Error: " <> displayException err)
