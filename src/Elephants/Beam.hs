{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
-- There is nothing to see here
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Elephants.Beam (runThis) where

import Control.Exception (Exception (displayException), catch)
import Control.Monad (void)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Time (LocalTime)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)
import Database.Beam.Postgres (ConnectInfo (..), Connection, Postgres, SqlError (..), defaultConnectInfo, runBeamPostgres, runBeamPostgresDebug)
import Database.PostgreSQL.Simple qualified as Simple
import Hardcoded qualified

-- database schema
data WarehouseDb f = WarehouseDb
  { product :: f (TableEntity ProductT)
  , category :: f (TableEntity CategoryT)
  , product_category :: f (TableEntity ProductCategoryT)
  , warehouse :: f (TableEntity WarehouseT)
  }
  deriving (Generic)
  deriving anyclass (Database Postgres)

warehouseDb :: DatabaseSettings Postgres WarehouseDb
warehouseDb =
  defaultDbSettings
    `withDbModification` dbModification
      { product_category =
          modifyTableFields
            tableModification
              { category_id = CategoryId (fieldNamed "category_id")
              , product_id = ProductId (fieldNamed "product_id")
              }
      , warehouse =
          modifyTableFields @WarehouseT
            tableModification
              { product_id = ProductId (fieldNamed "product_id")
              }
      }

-- product table
data ProductT f = Product
  { id :: Columnar f Int64
  , label :: Columnar f Text
  , description :: Columnar f (Maybe Text)
  }
  deriving (Generic)
  deriving anyclass (Beamable)

type Product = ProductT Identity
deriving instance Show Product

-- type ProductId = PrimaryKey ProductT Identity

instance Table ProductT where
  data PrimaryKey ProductT f = ProductId (Columnar f Int64)
    deriving (Generic)
    deriving anyclass (Beamable)
  primaryKey = ProductId . (.id)

-- category table
data CategoryT f = Category
  { id :: Columnar f Int64
  , label :: Columnar f Text
  }
  deriving (Generic)
  deriving anyclass (Beamable)

type Category = CategoryT Identity
deriving instance Show Category

instance Table CategoryT where
  data PrimaryKey CategoryT f = CategoryId (Columnar f Int64)
    deriving (Generic)
    deriving anyclass (Beamable)
  primaryKey = CategoryId . (.id)

-- mapping table
data ProductCategoryT f = ProductCategory
  { product_id :: PrimaryKey ProductT f
  , category_id :: PrimaryKey CategoryT f
  }
  deriving (Generic)
  deriving anyclass (Beamable)

instance Table ProductCategoryT where
  data PrimaryKey ProductCategoryT f = ProductCategoryId (PrimaryKey ProductT f) (PrimaryKey CategoryT f)
    deriving (Generic)
    deriving anyclass (Beamable)
  primaryKey = ProductCategoryId <$> (.product_id) <*> (.category_id)

-- warehouse table
data WarehouseT f = Warehouse
  { id :: Columnar f Int64
  , product_id :: PrimaryKey ProductT f
  , quantity :: Columnar f Int32
  , created :: Columnar f LocalTime
  , modified :: Columnar f LocalTime
  }
  deriving (Generic)
  deriving anyclass (Beamable)

instance Table WarehouseT where
  data PrimaryKey WarehouseT f = WarehouseId (Columnar f Int64)
    deriving (Generic)
    deriving anyclass (Beamable)
  primaryKey = WarehouseId . (.id)

runThis :: IO ()
runThis = do
  putStrLn "\nRunning beam"
  Simple.withConnect connectionInfo $ \connection -> do
    cleanUp connection
    insertStuff connection
    queryData connection
    insertWithTransaction connection
    queryWithJoins connection
    errors connection
  putStrLn "Done"

connectionInfo :: ConnectInfo
connectionInfo =
  defaultConnectInfo
    { connectHost = Hardcoded.host
    , connectDatabase = Hardcoded.database
    , connectUser = Hardcoded.user
    , connectPassword = Hardcoded.password
    }

cleanUp :: Connection -> IO ()
cleanUp connection =
  void $ Simple.execute_ connection "truncate warehouse, product_category, product, category"

insertStuff :: Connection -> IO ()
insertStuff connection = do
  -- Alternative for debugging:
  -- runBeamPostgresDebug putStrLn connection
  runBeamPostgres connection
    $ runInsert
    $ insert (warehouseDb.product)
    $ insertValues
      [ Product 1 "Wood Screw Kit 1" (Just "245-pieces")
      , Product 2 "Wood Screw Kit 2" Nothing
      ]
  putStrLn $ "Inserted products."

  result :: [Category] <-
    runBeamPostgres connection
      $ runInsertReturningList
      $ insert (warehouseDb.category)
      $ insertExpressions
        [ Category default_ "Screws"
        , Category default_ "Wood Screws"
        , Category default_ "Concrete Screws"
        ]

  putStrLn $ "Inserted categories: " <> show result

queryData :: Connection -> IO ()
queryData connection = runBeamPostgres connection $ do
  result1 <- query1
  liftIO $ putStrLn $ "Query 1: " <> show result1

  result2 <- query2 "Wood Screw Kit 1"
  liftIO $ putStrLn $ "Query 2: " <> show result2

  result3 <- query3 ["Wood Screw Kit 2" :: Text, "Wood Screw Kit 3"]
  liftIO $ putStrLn $ "Query 3: " <> show result3
 where
  query1 :: (MonadBeam Postgres m) => m [Product]
  query1 = do
    let allProducts = all_ (warehouseDb.product)
    runSelectReturningList $ select allProducts

  query2 label = runSelectReturningList $ select $ do
    aProduct <- all_ warehouseDb.product
    guard_ (aProduct.label ==. val_ label)
    pure (aProduct.label, aProduct.description)

  query3 labels =
    runSelectReturningList
      $ select
      $ filter_ (\p -> p.label `in_` predicate)
      $ all_ warehouseDb.product
   where
    predicate = val_ <$> labels

insertWithTransaction :: Connection -> IO ()
insertWithTransaction connection = Simple.withTransaction connection $ do
  [newProduct] :: [Product] <-
    runBeamPostgres connection
      $ runInsertReturningList
      $ insert (warehouseDb.product)
      $ insertExpressions [Product default_ "Drywall Screws Set" (just_ "8000pcs")]

  [newCategory] <-
    runBeamPostgres connection
      $ runInsertReturningList
      $ insert (warehouseDb.category)
      $ insertExpressions [Category default_ "Drywall Screws"]

  runBeamPostgresDebug putStrLn connection
    $ runInsert
    $ insert (warehouseDb.product_category)
    $ insertValues [ProductCategory (pk newProduct) (pk newCategory)]

  runBeamPostgres connection
    $ runInsert
    $ insert (warehouseDb.warehouse)
    $ insertExpressions [Warehouse default_ (val_ (pk newProduct)) 10 currentTimestamp_ currentTimestamp_]

  putStrLn $ "Insert with transaction"

queryWithJoins :: Connection -> IO ()
queryWithJoins connection = do
  result1 <- query1 3
  liftIO $ putStrLn $ "Query 1 with join: " <> show result1

  result2 <- query2 3
  liftIO $ putStrLn $ "Query 2 with join: " <> show result2
 where
  query1 quantity = runBeamPostgres connection
    $ runSelectReturningList
    $ select
    $ do
      warehouse <- all_ warehouseDb.warehouse
      aProduct <- related_ warehouseDb.product warehouse.product_id
      mapping <-
        leftJoin_
          (all_ warehouseDb.product_category)
          (\pc -> pc.product_id ==. primaryKey aProduct)
      category <-
        leftJoin_
          (all_ warehouseDb.category)
          (\c -> just_ (primaryKey c) ==. mapping.category_id)
      guard_ (warehouse.quantity >. quantity)
      pure (warehouse.quantity, aProduct.label, aProduct.description, category.label)

  query2 quantity = runBeamPostgres connection
    $ runSelectReturningList
    $ select
    $ do
      warehouse <- all_ warehouseDb.warehouse
      products <- related_ warehouseDb.product warehouse.product_id
      categories <- all_ warehouseDb.category
      (aProduct, category) <-
        productCategoryRelationship (pure products) (pure categories)
      guard_ (warehouse.quantity >. quantity)
      pure (warehouse.quantity, aProduct.label, aProduct.description, category.label)

  productCategoryRelationship :: ManyToMany Postgres WarehouseDb ProductT CategoryT
  productCategoryRelationship =
    manyToMany_ (warehouseDb.product_category) (.product_id) (.category_id)

errors :: Connection -> IO ()
errors connection = do
  insertDuplicateScrew
  insertDuplicateScrew
    `catch` (\err@SqlError{} -> putStrLn $ "Caught SQL Error: " <> displayException err)
 where
  insertDuplicateScrew =
    runBeamPostgres connection
      $ runInsert
      $ insert (warehouseDb.product)
      $ insertExpressions [Product default_ "Duplicate screw" nothing_]
