module Elephants.GenericPersistence (runThis) where

import Control.Exception (handle, displayException)
import Control.Monad (void)
import Data.Int (Int64)
import Data.Text (Text)
import           Database.GP
import           Database.HDBC.PostgreSQL (connectPostgreSQL)
import           GHC.Generics 
import Hardcoded qualified
import Data.Time.Clock (UTCTime, getCurrentTime)

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
getConnection = connect ExplicitCommit <$> connectPostgreSQL Hardcoded.connectionString 

cleanUp :: Conn -> IO ()
cleanUp connection = runRaw connection "truncate warehouse, product_category, product, category"


data Product = Product {id :: Int64, label :: Text, description :: Maybe Text}
  deriving (Show, Generic)

instance Entity Product where
  idField = "id"


newtype Category = Category {label :: Text}
  deriving (Show, Generic)
  deriving anyclass Entity

data CategoryWithId = CategoryWithId {id :: Int64, label :: Text}
  deriving (Show, Generic)

instance Entity CategoryWithId where
  tableName = "category"
  idField = "id"


insertStuff :: Conn -> IO ()
insertStuff connection = do
  let serial = 1 :: Int64 -- serial is used as a placeholder value for auto-incremented fields
  product1 <- insertReturning connection (Product serial "Wood Screw Kit 1" (Just "245-pieces"))
  putStrLn $ "Insert 1: " <> show product1

  product2 <- insertReturning connection $ Product serial "Wood Screw Kit 2" Nothing
  putStrLn $ "Insert 2: " <> show product2

  product3 <- insertReturning connection $ Product serial "Wood Screw Kit 3" Nothing
  putStrLn $ "Insert 3: " <> show product3

  product4 <- insertReturning connection $ Product serial "Wood Screw Kit 4" (Just "245-pieces")
  putStrLn $ "Insert 4: " <> show product4

  let categories = [Category "Screws", Category "Wood Screws", Category "Concrete Screws"]
  insertMany connection categories
  putStrLn $ "Insert 5: " <> show (length categories) <> " categories"
  commit connection

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


data Warehouse = Warehouse {product_id :: Int64, quantity :: Int, created :: UTCTime, modified :: UTCTime}
  deriving (Show, Generic, Entity)

data ProductCategory = ProductCategory {category_id :: Int64, product_id :: Int64}
  deriving (Show, Generic)
instance Entity ProductCategory where
  tableName = "product_category"

insertWithTransaction :: Conn -> IO ()
insertWithTransaction conn = withTransaction conn $ \connection -> do
  (Product productId _ _)  <- insertReturning connection (Product serial "Drywall Screws Set" (Just "8000pcs"))
  (CategoryWithId catId _) <- insertReturning connection (CategoryWithId serial "Drywall Screws")

  now <- getCurrentTime
  let warehouseEntry = Warehouse productId 10 now now
      prod2cat = ProductCategory catId productId
  insert connection warehouseEntry
  insert connection prod2cat

  putStrLn $ "Insert with transaction: " <> show warehouseEntry <> " and " <> show prod2cat

queryWithJoins :: Conn -> IO ()
queryWithJoins connection = do
  let stmt = [sql|
        select w.quantity, p.label, p.description, c.label
        from warehouse as w
        inner join product as p on w.product_id = p.id
        left outer join product_category as pc on p.id = pc.product_id
        left outer join category as c on c.id = pc.category_id
        where w.quantity > (?)|]

  -- perform a custom HDBC quickQuery and convert the resulting rows into a list of Listing objects
  listings <- entitiesFromRows @Listing connection =<< quickQuery connection stmt [toSql (3 :: Int)]
  putStrLn $ "Query with join: " <> show listings

data Listing = Listing {quantity :: Int, label :: Text, description :: Maybe Text, category :: Maybe Text}
  deriving (Show, Generic, Entity)

errors :: Conn -> IO ()
errors connection = do
  handle @PersistenceException (\err -> putStrLn $ "\nDuplicate Insert: " <> displayException err)
    $ void 
    $ do
      [Product prodId _ _] <- select @Product connection (field "label" =. ("Wood Screw Kit 1" :: Text))
      insert connection (Product prodId "Screwdriver" Nothing)
