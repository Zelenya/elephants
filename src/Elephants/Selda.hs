{-# LANGUAGE OverloadedLabels #-}
-- Nothing to see here
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Elephants.Selda (runThis) where

import Control.Monad.Catch (catch, displayException)
import Data.Int (Int32)
import Data.Text.IO qualified as Text
import Database.Selda
import Database.Selda.Debug (compile)
import Database.Selda.PostgreSQL (PG, PGConnectInfo (..), withPostgreSQL)
import Database.Selda.Unsafe (rawStm)
import Hardcoded qualified

-- product table
data Product = Product
  { id :: ID Product
  , label :: Text
  , description :: Maybe Text
  }
  deriving (Generic, Show)
  deriving anyclass (SqlRow)

productTable :: Table Product
productTable = table "product" [#id :- autoPrimary]

-- category table
data Category = Category
  { id :: ID Category
  , label :: Text
  }
  deriving (Generic)
  deriving anyclass (SqlRow)

categoryTable :: Table Category
categoryTable = table "category" [#id :- autoPrimary]

-- mapping table
data ProductCategory = ProductCategory
  { product_id :: ID Product
  , category_id :: ID Category
  }
  deriving (Generic)
  deriving anyclass (SqlRow)

mappingTable :: Table ProductCategory
mappingTable =
  table
    "product_category"
    [ #product_id :- foreignKey productTable #id
    , #category_id :- foreignKey categoryTable #id
    ]

-- warehouse table
data Warehouse = Warehouse
  { id :: ID Warehouse
  , product_id :: ID Product
  , quantity :: Int32
  , created :: UTCTime
  , modified :: UTCTime
  }
  deriving (Generic)
  deriving anyclass (SqlRow)

warehouseTable :: Table Warehouse
warehouseTable = table "warehouse" [#id :- autoPrimary, #product_id :- foreignKey productTable #id]

runThis :: IO ()
runThis = do
  putStrLn "\nRunning selda"
  withPostgreSQL connectionInfo $ do
    cleanUp
    insertStuff
    queryData
    insertWithTransaction
    queryWithJoins
    errors
  putStrLn "Done"

connectionInfo :: PGConnectInfo
connectionInfo =
  PGConnectInfo
    { pgHost = Hardcoded.host
    , pgPort = Hardcoded.portNumber
    , pgDatabase = Hardcoded.database
    , pgUsername = Just Hardcoded.user
    , pgPassword = Just Hardcoded.password
    , pgSchema = Nothing
    }

cleanUp :: SeldaM PG ()
cleanUp =
  rawStm "truncate warehouse, product_category, product, category"

insertStuff :: SeldaM PG ()
insertStuff = do
  productId <-
    insertWithPK
      productTable
      [ Product def "Wood Screw Kit 1" (Just "245-pieces")
      , Product def "Wood Screw Kit 2" Nothing
      ]
  liftIO $ putStrLn $ "Inserted product with id: " <> show productId

  rows <-
    insert
      categoryTable
      [Category def "Screws", Category def "Wood Screws", Category def "Concrete Screws"]
  liftIO $ putStrLn $ "Inserted categories: " <> show rows

queryData :: SeldaT PG IO ()
queryData = do
  -- elephants-exe: [SELDA BUG] fromSql: RowID column with non-int value: SqlInt32 ...
  -- result1 <- query selectProduct
  -- liftIO $ putStrLn $ "Query 1: " <> show result1

  result2 <- query select2
  liftIO $ putStrLn $ "Query 2: " <> show result2

  result3 <- query select3
  liftIO $ putStrLn $ "Query 3: " <> show result3
 where
  selectProduct :: Query t (Row t Product)
  selectProduct = select productTable

  select2 :: Query t (Col t Text :*: Col t (Maybe Text))
  select2 = do
    p <- selectProduct
    restrict (p ! #label .== "Wood Screw Kit 2")
    pure (p ! #label :*: p ! #description)

  select3 = do
    p <- selectProduct
    restrict (p ! #label `isIn` ["Wood Screw Kit 2", "Wood Screw Kit 3"])
    pure (p ! #label)

insertWithTransaction :: SeldaT PG IO ()
insertWithTransaction = transaction $ do
  productId <- insertWithPK productTable [Product def "Drywall Screws Set" (Just "8000pcs")]
  categoryId <- insertWithPK categoryTable [Category def "Drywall Screws"]
  insert_ mappingTable [ProductCategory productId categoryId]
  insert_ warehouseTable [Warehouse def productId 10 def def]
  liftIO $ putStrLn $ "Insert with transaction"

queryWithJoins :: SeldaM PG ()
queryWithJoins = do
  -- liftIO $ Text.putStrLn $ fst $ (compile join)
  result1 <- query join
  liftIO $ putStrLn $ "Query with join: " <> show result1
 where
  join :: Query s (Col s Int32 :*: (Col s Text :*: (Col s (Maybe Text) :*: Col s (Coalesce (Maybe Text)))))
  join = do
    w <- select warehouseTable
    p <- select productTable
    restrict (w ! #product_id .== p ! #id)

    pc <- leftJoin (\pc -> pc ! #product_id .== p ! #id) (select mappingTable)
    c <- leftJoin (\c -> just (c ! #id) .== pc ? #category_id) (select categoryTable)

    pure (w ! #quantity :*: p ! #label :*: p ! #description :*: c ? #label)

errors :: SeldaM PG ()
errors = do
  insertDuplicateScrew
  insertDuplicateScrew
    `catch` (\(err :: SeldaError) -> liftIO $ putStrLn $ "Caught Selda Error: " <> displayException err)
 where
  insertDuplicateScrew = insert_ productTable [Product def "Duplicate screw" Nothing]

data Listing = Listing {quantity :: Int, label :: Text, description :: Maybe Text, category :: Maybe Text}
  deriving (Show, Generic)
  deriving anyclass (SqlRow)
