{-# OPTIONS_GHC -Wno-missing-fields #-}

module Elephants.GenericPersistence (runThis) where

import Control.Exception (displayException, handle)
import Control.Monad (void)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.GP
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import GHC.Generics (Generic)
import Hardcoded qualified

data Product = Product {id :: Int64, label :: Text, description :: Maybe Text}
  deriving (Show, Generic)

instance Entity Product where
  idField = "id"

data Category = Category {id :: Int64, label :: Text}
  deriving (Show, Generic)

instance Entity Category where
  idField = "id"

data ProductCategory = ProductCategory {category_id :: Int64, product_id :: Int64}
  deriving (Show, Generic)

instance Entity ProductCategory where
  tableName :: String
  tableName = "product_category"
  autoIncrement = False

data Warehouse = Warehouse {product_id :: Int64, quantity :: Int, created :: UTCTime, modified :: UTCTime}
  deriving (Show, Generic)

instance Entity Warehouse where
  -- We ignore warehouse ids in the code
  autoIncrement = False

runThis :: IO ()
runThis = do
  putStrLn "\nRunning generic-persistence"
  connection <- getConnection
  cleanUp connection
  insertStuff connection
  queryData connection
  insertWithTransaction connection
  queryWithJoins connection
  errors connection
  disconnect connection
  putStrLn "Done"

getConnection :: IO Conn
getConnection = connect AutoCommit <$> connectPostgreSQL Hardcoded.connectionString

cleanUp :: Conn -> IO ()
cleanUp connection = runRaw connection "truncate warehouse, product_category, product, category"

insertStuff :: Conn -> IO ()
insertStuff connection = do
  product1 <- insert connection $ Product{label = "Wood Screw Kit 1", description = Just "245-pieces"}
  putStrLn $ "Insert 1: " <> show product1

  product2 <- insert connection $ Product{label = "Wood Screw Kit 2", description = Nothing}
  putStrLn $ "Insert 2: " <> show product2

  insertMany connection [Category{label = "Screws"}, Category{label = "Wood Screws"}, Category{label = "Concrete Screws"}]
  putStrLn $ "Inserted categories"

queryData :: Conn -> IO ()
queryData connection = do
  products1 <- select @Product connection allEntries
  putStrLn $ "Query 1: " <> show products1

  let labelWsk2 = "Wood Screw Kit 2" :: Text
      labelWsk3 = "Wood Screw Kit 3" :: Text
  products2 <- select @Product connection (field "label" =. labelWsk2)
  putStrLn $ "Query 2: " <> show products2

  products3 <- select @Product connection (field "label" `in'` [labelWsk2, labelWsk3])
  putStrLn $ "Query 3: " <> show products3

insertWithTransaction :: Conn -> IO ()
insertWithTransaction conn = withTransaction conn $ \connection -> do
  (Product productId _ _) <- insert connection (Product{label = "Drywall Screws Set", description = Just "8000pcs"})
  (Category catId _) <- insert connection (Category{label = "Drywall Screws"})
  now <- getCurrentTime
  _ <- insert connection (Warehouse productId 10 now now)
  _ <- insert connection (ProductCategory catId productId)
  putStrLn $ "Inserted with transaction"

queryWithJoins :: Conn -> IO ()
queryWithJoins connection = do
  let stmt =
        [sql|
        select w.quantity, p.label, p.description, c.label
        from warehouse as w
        inner join product as p on w.product_id = p.id
        left outer join product_category as pc on p.id = pc.product_id
        left outer join category as c on c.id = pc.category_id
        where w.quantity > (?)|]

  -- perform a custom HDBC quickQuery and convert the resulting rows into a list of Listing objects
  listings <- entitiesFromRows @Listing connection =<< quickQuery connection stmt [toSql (3 :: Int)]
  putStrLn $ "Query with join: " <> show listings

errors :: Conn -> IO ()
errors connection = do
  insertDuplicateScrew
  handle @PersistenceException (\err -> putStrLn $ "Caught SQL Error: " <> displayException err) insertDuplicateScrew
 where
  insertDuplicateScrew =
    void $ insert connection $ Product{label = "Duplicate screw", description = Nothing}

data Listing = Listing {quantity :: Int, label :: Text, description :: Maybe Text, category :: Maybe Text}
  deriving (Show, Generic)
  deriving anyclass (Entity)
