module Elephants.PostgresqlSimple (runThis) where

import Control.Exception (Exception (displayException), handle, throwIO)
import Control.Monad (void)
import Data.Int (Int64)
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ (sql)
import GHC.Generics (Generic)
import Hardcoded qualified

runThis :: IO ()
runThis = do
  putStrLn "\nRunning postgresql-simple"
  connection <- getConnection
  cleanUp connection
  insertStuff connection
  queryData connection
  insertWithTransaction connection
  queryWithJoins connection
  errors connection
  close connection
  putStrLn "Done"

getConnection :: IO Connection
getConnection =
  connect
    $ defaultConnectInfo
      { connectHost = Hardcoded.host
      , connectDatabase = Hardcoded.database
      , connectUser = Hardcoded.user
      , connectPassword = Hardcoded.password
      }

cleanUp :: Connection -> IO ()
cleanUp connection =
  void
    $ execute_
      connection
      "truncate warehouse, product_category, product, category"

insertStuff :: Connection -> IO ()
insertStuff connection = do
  insert1 <-
    execute
      connection
      "insert into product (label, description) values (?, ?)"
      ("Wood Screw Kit 1" :: Text, "245-pieces" :: Text)
  putStrLn $ "Insert 1: " <> show insert1 <> " record(s)"

  insert2 <-
    execute
      connection
      "insert into product (label) values (?)"
      (Only "Wood Screw Kit 2" :: Only Text)
  putStrLn $ "Insert 2: " <> show insert2 <> " record(s)"

  insert3 <-
    execute
      connection
      "insert into product (label) values (?)"
      ["Wood Screw Kit 3" :: Text]
  putStrLn $ "Insert 3: " <> show insert3 <> " record(s)"

  insert4 <-
    execute
      connection
      "insert into product (label, description) values (?, ?)"
      (BasicProduct "Wood Screw Kit 4" (Just "245-pieces"))
  putStrLn $ "Insert 4: " <> show insert4 <> " record(s)"

  insert5 <-
    executeMany
      connection
      "insert into category (label) values (?)"
      [Category "Screws", Category "Wood Screws", Category "Concrete Screws"]
  putStrLn $ "Insert 5: " <> show insert5 <> " record(s)"

queryData :: Connection -> IO ()
queryData connection = do
  query1 :: [(Int64, Text, Maybe Text)] <-
    query_ connection "select id, label, description from product"
  putStrLn $ "Query 1: " <> show query1

  query2 :: [BasicProduct] <-
    query
      connection
      "select label, description from product where label = ? "
      (Only "Wood Screw Kit 2" :: Only Text)
  putStrLn $ "Query 2: " <> show query2

  query3 :: [BasicProduct] <-
    query connection "select label, description from product where label in ?"
      $ Only (In ["Wood Screw Kit 2" :: Text, "Wood Screw Kit 3"])
  putStrLn $ "Query 3: " <> show query3

insertWithTransaction :: Connection -> IO ()
insertWithTransaction connection = withTransaction connection $ do
  productIds :: [Only Int64] <-
    query
      connection
      "insert into product (label, description) values (?, ?) returning id"
      (BasicProduct "Drywall Screws Set" (Just "8000pcs"))

  categoryIds :: [Only Int64] <-
    query
      connection
      "insert into category (label) values (?) returning id"
      (Category "Drywall Screws")

  mapping <- case (productIds, categoryIds) of
    ([Only productId], [Only categoryId]) -> do
      _ <-
        execute
          connection
          "insert into warehouse (product_id, quantity, created, modified) values (?, ?, now(), now())"
          (productId, 10 :: Int)

      execute
        connection
        "insert into product_category (category_id, product_id) values (?, ?)"
        (categoryId, productId)
    _ ->
      throwIO $ userError "Failed to insert product/category"

  putStrLn $ "Insert with transaction: " <> show mapping <> " mapping(s)"

queryWithJoins :: Connection -> IO ()
queryWithJoins connection = do
  result :: [Listing] <-
    query
      connection
      [sql|
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
      [3 :: Int]
  putStrLn $ "Query with join: " <> show result

errors :: Connection -> IO ()
errors connection = do
  handle @FormatError (\err -> putStrLn $ "Wrong query formatting: " <> displayException err)
    $ void
    $ execute
      connection
      "INSERT INTO category (label) VALUES (?)"
      ("One" :: Text, "Two" :: Text)

  handle @ResultError (\err -> putStrLn $ "Wrong result type: " <> displayException err)
    $ let result :: IO [(Text, Text)] =
            query_ connection "select label, description from product"
       in void result

  handle @ResultError (\err -> putStrLn $ "Wrong result type: " <> displayException err)
    $ let result :: IO [BasicProduct] =
            query_ connection "select id from product"
       in void result

  handle @QueryError (\err -> putStrLn $ "Wrong function: " <> displayException err)
    $ void
    $ execute_
      connection
      "INSERT INTO category (label) VALUES ('Deck Screws') returning id"

  handle @SqlError (\err -> putStrLn $ "Wrong sql: " <> displayException err)
    $ let result :: IO [BasicProduct] = query_ connection "select I have no idea what I'm doing"
       in void result

data BasicProduct = BasicProduct {label :: Text, description :: Maybe Text}
  deriving (Show, Generic)
  deriving anyclass (ToRow, FromRow)

newtype Category = Category {label :: Text}
  deriving (Show, Generic)
  deriving anyclass (ToRow, FromRow)

data Listing = Listing {quantity :: Int, label :: Text, description :: Maybe Text, category :: Maybe Text}
  deriving (Show, Generic)
  deriving anyclass (ToRow, FromRow)
