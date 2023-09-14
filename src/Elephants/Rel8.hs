{-# LANGUAGE TypeFamilies #-}

module Elephants.Rel8 (runThis) where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Time (LocalTime)
import GHC.Generics (Generic)
import Hasql.Connection (Connection, ConnectionError, acquire, release, settings)
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session (QueryError, run, statement)
import Hasql.Statement (Statement (..))
import Hasql.Transaction qualified as Transaction
import Hasql.Transaction.Sessions (IsolationLevel (..), Mode (..), transaction)
import Rel8
import Prelude hiding (filter, null)

import Hardcoded qualified

-- product table
newtype ProductId = ProductId Int64
  deriving newtype (DBEq, DBType, Eq, Show)

data Product f = Product
  { id :: Column f ProductId
  , label :: Column f Text
  , description :: Column f (Maybe Text)
  }
  deriving (Generic)
  deriving anyclass (Rel8able)

-- Imagine this is just "deriving (Show)"
deriving stock instance (f ~ Result) => Show (Product f)

productSchema :: TableSchema (Product Name)
productSchema =
  TableSchema
    { name = "product"
    , schema = Nothing
    , columns =
        Product
          { id = "id"
          , label = "label"
          , description = "description"
          }
    }

-- category table
newtype CategoryId = CategoryId Int64
  deriving newtype (DBEq, DBType, Eq, Show)

data Category f = Category
  { id :: Column f CategoryId
  , label :: Column f Text
  }
  deriving (Generic)
  deriving anyclass (Rel8able)

categorySchema :: TableSchema (Category Name)
categorySchema =
  TableSchema
    { name = "category"
    , schema = Nothing
    , columns = namesFromLabels @(Category Name)
    }

-- mapping table
data ProductCategory f = ProductCategory
  { product_id :: Column f ProductId
  , category_id :: Column f CategoryId
  }
  deriving (Generic)
  deriving anyclass (Rel8able)

productCategorySchema :: TableSchema (ProductCategory Name)
productCategorySchema =
  TableSchema
    { name = "product_category"
    , schema = Nothing
    , columns = namesFromLabels @(ProductCategory Name)
    }

-- warehouse table
data Warehouse f = Warehouse
  { id :: Column f Int64
  , product_id :: Column f ProductId
  , quantity :: Column f Int32
  , created :: Column f LocalTime
  , modified :: Column f LocalTime
  }
  deriving (Generic)
  deriving anyclass (Rel8able)

warehouseSchema :: TableSchema (Warehouse Name)
warehouseSchema =
  TableSchema
    { name = "warehouse"
    , schema = Nothing
    , columns = namesFromLabels @(Warehouse Name)
    }

runThis :: IO ()
runThis = do
  putStrLn "\nRunning rel8"
  -- Do as I say, not as I do
  Right connection <- getConnection
  Right _ <- cleanUp connection
  insertStuff connection
  queryData connection
  insertWithTransaction connection
  queryWithJoins connection
  errors connection
  release connection
  putStrLn "Done"

getConnection :: IO (Either ConnectionError Connection)
getConnection =
  acquire $ settings Hardcoded.host Hardcoded.portNumber Hardcoded.user Hardcoded.password Hardcoded.database

cleanUp :: Connection -> IO (Either QueryError ())
cleanUp connection = run cleanUpSession connection
 where
  cleanUpSession = statement () $ Statement rawSql E.noParams D.noResult True
  rawSql = "truncate warehouse, product_category, product, category"

insertStuff :: Connection -> IO ()
insertStuff connection = do
  result1 <- run (statement () insert1) connection
  putStrLn $ "Inserted products: " <> show result1

  result2 <- run (statement () insert2) connection
  putStrLn $ "Inserted categories: " <> show result2
 where
  insert1 :: Statement () [ProductId]
  insert1 =
    insert
      $ Insert
        { into = productSchema
        , rows =
            values
              [ Product unsafeDefault "Wood Screw Kit 1" null
              , Product unsafeDefault "Wood Screw Kit 2" (lit $ Just "245-pieces")
              ]
        , returning = Projection (.id)
        , onConflict = Abort
        }

  insert2 :: Statement () Int64
  insert2 =
    insert
      $ Insert
        { into = categorySchema
        , rows =
            values
              [ Category unsafeDefault "Screws"
              , Category unsafeDefault "Wood Screws"
              , Category unsafeDefault "Concrete Screws"
              ]
        , returning = NumberOfRowsAffected
        , onConflict = Abort
        }

queryData :: Connection -> IO ()
queryData connection = do
  result1 <- run (statement () select1) connection
  putStrLn $ "Query 1: " <> show result1

  result2 <- run (statement () select2) connection
  putStrLn $ "Query 2: " <> show result2

  result3 <- run (statement () select3) connection
  putStrLn $ "Query 3: " <> show result3
 where
  select1 :: Statement () [Product Result]
  select1 = select $ each productSchema

  select2 :: Statement () [(Text, Maybe Text)]
  select2 = select $ do
    p <- each productSchema
    where_ $ p.label ==. "Wood Screw Kit 2"
    pure (p.label, p.description)

  select3 :: Statement () [Text]
  select3 = select $ do
    p <- each productSchema
    where_ $ p.label `in_` ["Wood Screw Kit 2", "Wood Screw Kit 3"]
    pure p.label

insertWithTransaction :: Connection -> IO ()
insertWithTransaction connection = do
  result <- run (transaction Serializable Write insertAll) connection
  putStrLn $ "Insert with transaction: " <> show result
 where
  insertAll = do
    productIds <-
      Transaction.statement ()
        $ insert
        $ Insert
          { into = productSchema
          , rows = values [Product unsafeDefault "Drywall Screws Set" (lit $ Just "8000pcs")]
          , returning = Projection (.id)
          , onConflict = Abort
          }

    categoryIds <-
      Transaction.statement ()
        $ insert
        $ Insert
          { into = categorySchema
          , rows = values [Category unsafeDefault "Drywall Screws"]
          , returning = Projection (.id)
          , onConflict = Abort
          }

    -- Because `statement`s return multiple ids and `Insert`s take multiple rows
    let mappings = do
          productId <- productIds
          categoryId <- categoryIds
          pure $ ProductCategory (lit productId) (lit categoryId)

    let listings = do
          productId <- productIds
          pure $ Warehouse unsafeDefault (lit productId) 10 unsafeDefault unsafeDefault

    _ <-
      Transaction.statement ()
        $ insert
        $ Insert
          { into = productCategorySchema
          , rows = values mappings
          , returning = NumberOfRowsAffected
          , onConflict = Abort
          }

    Transaction.statement ()
      $ insert
      $ Insert
        { into = warehouseSchema
        , rows = values listings
        , returning = NumberOfRowsAffected
        , onConflict = Abort
        }

queryWithJoins :: Connection -> IO ()
queryWithJoins connection = do
  -- putStrLn $ showQuery joinQuery
  result1 <- run (statement () join) connection
  putStrLn $ "Query with join: " <> show result1
 where
  join :: Statement () [(Int32, Text, Maybe Text, Maybe Text)]
  join = select joinQuery

  joinQuery = do
    w <- each warehouseSchema
    p <- productsInWarehouse w
    pc <- optional $ mappingsForProduct p
    c <- traverseMaybeTable categoriesForMapping pc
    where_ $ w.quantity >. 3
    let category = maybeTable null (nullify . (.label)) c
    pure (w.quantity, p.label, p.description, category)

  productsInWarehouse :: Warehouse Expr -> Query (Product Expr)
  productsInWarehouse w =
    each productSchema >>= filter (\p -> p.id ==. w.product_id)

  mappingsForProduct :: Product Expr -> Query (ProductCategory Expr)
  mappingsForProduct p = do
    each productCategorySchema >>= filter (\pc -> pc.product_id ==. p.id)

  categoriesForMapping :: ProductCategory Expr -> Query (Category Expr)
  categoriesForMapping pc =
    each categorySchema >>= filter (\c -> c.id ==. pc.category_id)

errors :: Connection -> IO ()
errors connection = do
  Left failure <-
    run insertDuplicateScrew connection
      >> run insertDuplicateScrew connection
  putStrLn $ "Constraint violation (Left): " <> show failure
 where
  insertDuplicateScrew =
    statement ()
      $ insert
      $ Insert
        { into = productSchema
        , rows = values [Product unsafeDefault "Duplicate screw" null]
        , returning = NumberOfRowsAffected
        , onConflict = Abort
        }
