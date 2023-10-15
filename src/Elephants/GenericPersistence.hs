module Elephants.GenericPersistence (runThis) where

import Control.Exception (handle, displayException)
import Control.Monad (void)
import Data.Int (Int64)
import Data.Text (Text)
import           Database.GP
import           Database.HDBC.PostgreSQL (connectPostgreSQL)
import           GHC.Generics 
import Hardcoded qualified
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime


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


data Product = Product {label :: Text, description :: Maybe Text}
  deriving (Show, Generic, Entity)

data ProductWithId = ProductWithId {id :: Int64,  label :: Text, description :: Maybe Text}
  deriving (Show, Generic)
  
instance Entity ProductWithId where
  tableName = "product"
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
  let product1 = Product "Wood Screw Kit 1" (Just "245-pieces")
  insert connection product1
  putStrLn   "Insert 1: inserted 1 record"

  let product2 = Product "Wood Screw Kit 2" Nothing
  insert connection product2
  putStrLn "Insert 2: inserted 1 record"

  let product3 = Product "Wood Screw Kit 3" Nothing
  insert connection product3
  putStrLn   "Insert 3: inserted 1 record"

  let product4 = Product "Wood Screw Kit 4" (Just "245-pieces")
  insert connection product4
  putStrLn   "Insert 4: inserted 1 record"

  let categories = [Category "Screws", Category "Wood Screws", Category "Concrete Screws"]
  insertMany connection categories
  putStrLn $ "Insert 5: inserted " <> show (length categories) <> " records"

queryData :: Conn -> IO ()
queryData connection = do
  products1 <- select @ProductWithId connection  allEntries
  putStrLn $ "Query 1: " <> show products1
  
  let labelWsk2 = "Wood Screw Kit 2" :: Text 
  products2 <- select @Product connection (field "label" =. labelWsk2)
  putStrLn $ "Query 2: " <> show products2

  let labelWsk3 = "Wood Screw Kit 3" :: Text
  products3 <- select @Product connection (field "label" `in'` [labelWsk2, labelWsk3])
  putStrLn $ "Query 3: " <> show products3


data Warehouse = Warehouse {product_id :: Int64, quantity :: Int, created :: LocalTime, modified :: LocalTime}
  deriving (Show, Generic, Entity)

data ProductCategory = ProductCategory {category_id :: Int64, product_id :: Int64}
  deriving (Show, Generic)
instance Entity ProductCategory where
  tableName = "product_category"

insertWithTransaction :: Conn -> IO ()
insertWithTransaction conn = withTransaction conn $ \connection -> do
  (ProductWithId prodId _ _) <- insertReturning connection (ProductWithId serial "Drywall Screws Set" (Just "8000pcs"))
  (CategoryWithId catId _)   <- insertReturning connection (CategoryWithId serial "Drywall Screws")

  now <- getCurrentLocalTime
  let warehouseEntry = Warehouse prodId 10 now now
      prod2cat = ProductCategory catId prodId
  insert connection warehouseEntry
  insert connection prod2cat

  putStrLn $ "Insert with transaction: " <> show warehouseEntry <> " and " <> show prod2cat


data Listing = Listing {quantity :: Int, label :: Text, description :: Maybe Text, category :: Maybe Text}
  deriving (Show, Generic, Entity)

queryWithJoins :: Conn -> IO ()
queryWithJoins connection = do
  let stmt = [sql|
        select
          w.quantity,
          p.label,
          p.description,
          c.label
        from warehouse as w
        inner join product as p on w.product_id = p.id
        left outer join product_category as pc on p.id = pc.product_id
        left outer join category as c on c.id = pc.category_id
        where w.quantity > (?)|]

  -- perform a custom HDBC quickQuery call
  resultRows <- quickQuery connection stmt [toSql (3 :: Int)]

  -- convert the resulting rows into a list of Listing objects
  result <- entitiesFromRows @Listing connection resultRows
  putStrLn $ "Query with join: " <> show result


errors :: Conn -> IO ()
errors connection = do
  handle @PersistenceException (\err -> putStrLn $ "\nNo mapping for field: " <> displayException err)
    $ void 
    $ select @Product connection (field "non-existing-field" =. ("test" :: Text))

  handle @PersistenceException (\err -> putStrLn $ "\nDuplicate Insert: " <> displayException err)
    $ void 
    $ do
      [ProductWithId prodId _ _] <- select @ProductWithId connection (field "label" =. ("Wood Screw Kit 1" :: Text))
      insert connection (ProductWithId prodId "Screwdriver" Nothing)

  handle @PersistenceException (\err -> do putStrLn $ "no table for entity: " <> displayException err; rollback connection)
    $ void 
    $ select @ProductWithoutTable connection allEntries


getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = do
  t <- getCurrentTime
  tz <- getCurrentTimeZone
  return $ utcToLocalTime tz t


data ProductWithoutTable = ProductWithoutTable {label :: Text, description :: Maybe Text}
  deriving (Show, Generic, Entity)


serial :: Int64
serial = 0