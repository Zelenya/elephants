{-# LANGUAGE LambdaCase #-}

module Elephants.Opaleye (runThis) where

import Control.Exception (Exception (displayException), catch)
import Control.Monad (void)
import Data.Int (Int64)
import Data.Profunctor.Product (p5)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import Database.PostgreSQL.Simple (ConnectInfo (..), Connection, SqlError (..), defaultConnectInfo)
import Database.PostgreSQL.Simple qualified as Simple
import Hardcoded qualified
import Opaleye

-- product table
newtype ProductId' a = ProductId a
$(makeAdaptorAndInstance "pProductId" ''ProductId')
type ProductId = ProductId' Int
deriving instance Show ProductId
type ProductIdField = ProductId' (Field SqlInt4)

data Product' a b c = Product {pId :: a, pLabel :: b, pDescription :: c}
type Product = Product' ProductId Text (Maybe Text)
deriving instance Show Product

type ProductFieldWrite = Product' (ProductId' (Maybe (Field SqlInt4))) (Field SqlText) (FieldNullable SqlText)
type ProductField = Product' (ProductIdField) (Field SqlText) (FieldNullable SqlText)
$(makeAdaptorAndInstance "pProduct" ''Product')

productTable :: Table ProductFieldWrite ProductField
productTable =
  table "product"
    $ pProduct
      Product
        { pId = pProductId $ ProductId $ tableField "id"
        , pLabel = tableField "label"
        , pDescription = tableField "description"
        }

-- category table
newtype CategoryId' a = CategoryId a
$(makeAdaptorAndInstance "pCategoryId" ''CategoryId')
type CategoryId = CategoryId' Int
deriving instance Show CategoryId
type CategoryIdField = CategoryId' (Field SqlInt4)

data Category' a b = Category {cId :: a, cLabel :: b}
type Category = Category' CategoryId Text
deriving instance Show Category

type CategoryField = Category' CategoryIdField (Field SqlText)
$(makeAdaptorAndInstance "pCategory" ''Category')

categoryTable :: Table CategoryField CategoryField
categoryTable =
  table "category"
    $ pCategory
      Category
        { cId = pCategoryId (CategoryId (tableField "id"))
        , cLabel = tableField "label"
        }

-- mapping table
data Mapping' a b = Mapping {productId :: a, categoryId :: b}
type Mapping = Mapping' ProductId CategoryId
deriving instance Show Mapping

type MappingField = Mapping' ProductIdField CategoryIdField
$(makeAdaptorAndInstance "pMapping" ''Mapping')

productCategoryTable :: Table MappingField MappingField
productCategoryTable =
  table "product_category"
    $ pMapping
      Mapping
        { productId = pProductId (ProductId (tableField "product_id"))
        , categoryId = pCategoryId (CategoryId (tableField "category_id"))
        }

-- warehouse table
warehouseTable ::
  Table
    (Maybe (Field SqlInt4), ProductIdField, Field SqlInt4, Maybe (Field SqlTimestamp), Maybe (Field SqlTimestamp))
    (Field SqlInt4, ProductIdField, Field SqlInt4, Field SqlTimestamp, Field SqlTimestamp)
warehouseTable =
  table "warehouse"
    $ p5
      ( tableField "id"
      , pProductId (ProductId (tableField "product_id"))
      , tableField "quantity"
      , tableField "created"
      , tableField "modified"
      )

runThis :: IO ()
runThis = do
  putStrLn "\nRunning opaleye"
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
  result1 <- runInsert connection insert1
  putStrLn $ "Inserted products: " <> show result1

  result2 <- runInsert connection insert2
  putStrLn $ "Inserted categories: " <> show result2
 where
  insert1 :: Insert [ProductId]
  insert1 =
    Insert
      { iTable = productTable
      , iRows =
          [ Product (ProductId Nothing) "Wood Screw Kit 1" (maybeToNullable Nothing)
          , Product (ProductId Nothing) "Wood Screw Kit 2" (maybeToNullable (Just "245-pieces"))
          ]
      , iReturning = rReturning (\(p :: ProductField) -> p.pId)
      , iOnConflict = Nothing
      }

  insert2 :: Insert Int64
  insert2 =
    Insert
      { iTable = categoryTable
      , iRows = [Category (CategoryId 1) "Screws", Category (CategoryId 2) "Wood Screws", Category (CategoryId 3) "Concrete Screws"]
      , iReturning = rCount
      , iOnConflict = Nothing
      }

queryData :: Connection -> IO ()
queryData connection = do
  result1 :: [Product] <- runSelect connection selectProduct
  putStrLn $ "Query 1: " <> show result1

  result2 :: [(Text, Maybe Text)] <- runSelect connection select2
  putStrLn $ "Query 2: " <> show result2

  result3 :: [Text] <- runSelect connection select3
  putStrLn $ "Query 3: " <> show result3
 where
  selectProduct :: Select ProductField
  selectProduct = selectTable productTable

  select2 :: Select (Field SqlText, FieldNullable SqlText)
  select2 = do
    (Product _ aLabel description) <- selectProduct
    where_ (aLabel .== "Wood Screw Kit 2")
    pure (aLabel, description)

  select3 :: Select (Field SqlText)
  select3 = do
    p <- selectProduct
    where_ $ in_ ["Wood Screw Kit 2", "Wood Screw Kit 3"] p.pLabel
    pure p.pLabel

insertWithTransaction :: Connection -> IO ()
insertWithTransaction connection = Simple.withTransaction connection $ do
  [newProduct] :: [ProductId] <-
    runInsert connection
      $ Insert
        { iTable = productTable
        , iRows = [Product (ProductId Nothing) "Drywall Screws Set" (maybeToNullable $ Just "8000pcs")]
        , iReturning = rReturning (\(p :: ProductField) -> p.pId)
        , iOnConflict = Nothing
        }

  [newCategory] :: [CategoryId] <-
    runInsert connection
      $ Insert
        { iTable = categoryTable
        , iRows = [Category (CategoryId 123) "Drywall Screws"]
        , iReturning = rReturning (.cId)
        , iOnConflict = Nothing
        }

  void
    $ runInsert connection
    $ Insert
      { iTable = productCategoryTable
      , iRows = [Mapping (toFields newProduct) (toFields newCategory)]
      , iReturning = rCount
      , iOnConflict = Nothing
      }

  void
    $ runInsert connection
    $ Insert
      { iTable = warehouseTable
      , iRows = [(Nothing, (toFields newProduct), 10, Nothing, Nothing)]
      , iReturning = rCount
      , iOnConflict = Nothing
      }

  putStrLn $ "Insert with transaction"

queryWithJoins :: Connection -> IO ()
queryWithJoins connection = do
  -- putStrLn $ fromMaybe "EMPTY" $ showSql $ join
  result1 <- runSelectI connection $ join
  putStrLn $ "Query with join: " <> show result1
 where
  join :: Select (Field SqlInt4, Field SqlText, FieldNullable SqlText, MaybeFields (Field SqlText))
  join = do
    (_, wProductId, quantity, _, _) <- selectTable warehouseTable
    p <- selectTable productTable
    mpc <- optional $ do
      pc <- selectTable productCategoryTable
      where_ $ pc.productId .=== p.pId
      pure pc
    mc <- optional $ do
      c <- selectTable categoryTable
      where_ $ isJustAnd mpc $ \pc -> c.cId .=== pc.categoryId
      pure c

    where_ $ wProductId .=== p.pId
    where_ $ quantity .> 3

    let category = cLabel <$> mc
    pure (quantity, p.pLabel, p.pDescription, category)

  -- This will be added to Opaleye in the future
  isJustAnd :: MaybeFields a -> (a -> Field SqlBool) -> Field SqlBool
  isJustAnd ma cond = matchMaybe ma $ \case
    Nothing -> sqlBool False
    Just a -> cond a

errors :: Connection -> IO ()
errors connection = do
  insertDuplicateScrew
  insertDuplicateScrew
    `catch` (\err@SqlError{} -> putStrLn $ "Caught SQL Error: " <> displayException err)
 where
  insertDuplicateScrew =
    void
      $ runInsert connection
      $ Insert
        { iTable = productTable
        , iRows = [Product (ProductId Nothing) "Duplicate screw" (maybeToNullable Nothing)]
        , iReturning = rCount
        , iOnConflict = Nothing
        }
