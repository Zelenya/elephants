module Elephants.Hasql (runThis) where

import Data.Functor.Contravariant ((>$<))
import Data.Int (Int32, Int64)
import Data.List qualified as List
import Data.Profunctor (rmap)
import Data.Text (Text)
import Data.Tuple.Curry (uncurryN)
import Data.Vector (Vector, fromList)
import Hardcoded qualified
import Hasql.Connection (Connection, ConnectionError, acquire, release, settings)
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session (QueryError, Session, run, statement)
import Hasql.Statement (Statement (..))
import Hasql.TH (maybeStatement, resultlessStatement, rowsAffectedStatement, singletonStatement, vectorStatement)
import Hasql.Transaction (Transaction)
import Hasql.Transaction qualified as Transaction
import Hasql.Transaction.Sessions (IsolationLevel (..), Mode (..), transaction)

runThis :: IO ()
runThis = do
  putStrLn "\nRunning hasql"
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
  cleanUpSession :: Session ()
  cleanUpSession = statement () cleanUpStatement

  cleanUpStatement :: Statement () ()
  cleanUpStatement = Statement rawSql E.noParams D.noResult True

  rawSql = "truncate warehouse, product_category, product, category"

insertStuff :: Connection -> IO ()
insertStuff connection = do
  inserts <- run insertStuffSession connection
  putStrLn $ "Insert result: " <> show inserts
 where
  insertStuffSession :: Session Int64
  insertStuffSession = do
    insert1 <- statement ("Wood Screw Kit 1", Just "245-pieces") insertProduct1
    insert2 <- statement (BasicProduct "Wood Screw Kit 2" Nothing) insertProduct2
    let categories = [Category "Screws", Category "Wood Screws", Category "Concrete Screws"]
    insert3 <- statement (fromList categories) insertManyCategories
    pure $ insert1 + insert2 + insert3

  insertProductSql = "insert into product (label, description) values ($1, $2)"

  insertProduct1 :: Statement (Text, Maybe Text) Int64
  insertProduct1 = Statement insertProductSql rawParams D.rowsAffected True

  rawParams =
    (fst >$< E.param (E.nonNullable E.text))
      <> (snd >$< E.param (E.nullable E.text))

  insertProduct2 :: Statement BasicProduct Int64
  insertProduct2 =
    Statement insertProductSql basicProductParams D.rowsAffected True

  basicProductParams :: E.Params BasicProduct
  basicProductParams =
    ((.label) >$< E.param (E.nonNullable E.text))
      <> ((.description) >$< E.param (E.nullable E.text))

  insertManyCategories :: Statement (Vector Category) Int64
  insertManyCategories =
    Statement insertManyCategoriesSql categoryParams D.rowsAffected True

  insertManyCategoriesSql =
    "insert into category (label) select * from unnest ($1)"

  categoryParams :: E.Params (Vector Category)
  categoryParams =
    E.param
      $ E.nonNullable
      $ E.array
      $ E.dimension List.foldl'
      $ categoryArray

  categoryArray :: E.Array Category
  categoryArray = (.label) >$< (E.element $ E.nonNullable E.text)

queryData :: Connection -> IO ()
queryData connection = do
  query1 <- run session1 connection
  putStrLn $ "Query 1: " <> show query1

  query2 <- run session2 connection
  putStrLn $ "Query 2: " <> show query2

  query3 <- run session3 connection
  putStrLn $ "Query 3: " <> show query3

  query4 <- run session4 connection
  putStrLn $ "Query 4: " <> show query4
 where
  session1 :: Session [(Int64, Text, Maybe Text)]
  session1 =
    statement ()
      $ Statement
        "select id, label, description from product"
        E.noParams
        decoder1
        True

  decoder1 =
    D.rowList
      $ (,,)
      <$> D.column (D.nonNullable D.int8)
      <*> D.column (D.nonNullable D.text)
      <*> D.column (D.nullable D.text)

  session2 :: Session (Text, Maybe Text)
  session2 = statement () statement2

  statement2 :: Statement () (Text, Maybe Text)
  statement2 =
    [singletonStatement|
      select label :: text, description :: text? from product limit 1
    |]

  session3 :: Session (Maybe BasicProduct)
  session3 = statement "Wood Screw Kit 2" statement3

  statement3 :: Statement Text (Maybe BasicProduct)
  statement3 =
    rmap
      (fmap (uncurryN BasicProduct))
      -- aka (\result -> fmap (\(a, b) -> (BasicProduct a b)) result)
      [maybeStatement|
        select label :: text, description :: text?
        from product
        where label = $1 :: text
      |]

  session4 :: Session (Vector BasicProduct)
  session4 = statement (fromList ["Wood Screw Kit 1", "Wood Screw Kit 2"]) statement4

  statement4 :: Statement (Vector Text) (Vector BasicProduct)
  statement4 =
    rmap
      (fmap (uncurryN BasicProduct))
      [vectorStatement|
        select label :: text, description :: text?
        from product
        where label = ANY($1 :: text[])
      |]

insertWithTransaction :: Connection -> IO ()
insertWithTransaction connection = do
  let listing = FullProduct "Drywall Screws Set" (Just "8000pcs") "Drywall Screws"
  mapping <- run (transaction Serializable Write $ insertAll listing) connection
  putStrLn $ "Insert with transaction: " <> show mapping
 where
  insertAll :: FullProduct -> Transaction Int64
  insertAll listing = do
    productId <- Transaction.statement (listing.label, listing.description) insertProduct
    categoryId <- Transaction.statement listing.category insertCategory
    _ <- Transaction.statement (productId) insertListing
    ids <- Transaction.statement (productId, categoryId) insertMapping
    pure ids

  insertProduct :: Statement (Text, Maybe Text) Int64
  insertProduct =
    [singletonStatement|
      insert into product (label, description) values ($1 :: text, $2 :: text?) returning id :: int8
    |]

  insertCategory :: Statement Text Int64
  insertCategory =
    [singletonStatement|
      insert into category (label) values ($1 :: text) returning id :: int8
    |]

  insertListing :: Statement Int64 ()
  insertListing =
    [resultlessStatement|
      insert into warehouse (product_id, quantity, created, modified) values ($1 :: int8, 10, now(), now())
    |]

  insertMapping :: Statement (Int64, Int64) Int64
  insertMapping =
    [rowsAffectedStatement|
      insert into product_category (product_id, category_id) values ($1 :: int8, $2 :: int8)
    |]

queryWithJoins :: Connection -> IO ()
queryWithJoins connection = do
  result <- run (statement 3 listings) connection
  putStrLn $ "Query with join: " <> show result
 where
  listings :: Statement Int32 (Vector Listing)
  listings =
    rmap
      (fmap (uncurryN Listing))
      [vectorStatement|
      select
          w.quantity :: int,
          p.label :: text,
          p.description :: text?,
          c.label :: text?
        from warehouse as w
        inner join product as p on w.product_id = p.id
        left outer join product_category as pc on p.id = pc.product_id
        left outer join category as c on c.id = pc.category_id
        where w.quantity > $1 :: int4
    |]

errors :: Connection -> IO ()
errors connection = do
  Left failure1 <- run (statement () failsBecauseNoResults) connection
  putStrLn $ "Wrong statement function (Left): " <> show failure1

  Left failure2 <- run (statement () failsBecauseResultless) connection
  putStrLn $ "Wrong statement function (Left): " <> show failure2

  Left failure3 <-
    run (statement "Duplicate screw" inserProduct) connection
      >> run (statement "Duplicate screw" inserProduct) connection
  putStrLn $ "Constraint violation (Left): " <> show failure3
 where
  -- This won't compile
  -- wontCompile = [singletonStatement| select I have no idea what I'm doing |]

  -- This should use maybeStatement
  failsBecauseNoResults :: Statement () (Text)
  failsBecauseNoResults =
    [singletonStatement|
        select label :: text from product where 1 = 0
    |]

  -- This should use resultlessStatement
  failsBecauseResultless :: Statement () ()
  failsBecauseResultless =
    [singletonStatement|
      insert into product (label) values ('this insert fails')
    |]

  inserProduct :: Statement Text ()
  inserProduct =
    [singletonStatement|
      insert into product (label) values ($1 :: text)
    |]

data BasicProduct = BasicProduct {label :: Text, description :: Maybe Text}
  deriving (Show)

newtype Category = Category {label :: Text}
  deriving (Show)

data FullProduct = FullProduct {label :: Text, description :: Maybe Text, category :: Text}
  deriving (Show)

data Listing = Listing {quanity :: Int32, label :: Text, description :: Maybe Text, category :: Maybe Text}
  deriving (Show)
