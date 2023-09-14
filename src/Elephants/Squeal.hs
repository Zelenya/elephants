{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}

module Elephants.Squeal (runThis) where

import Control.Monad (join)
import Control.Monad.Catch (displayException, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int (Int32)
import Data.Text (Text)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Hardcoded qualified
import Squeal.PostgreSQL

type ProductColumns =
  '[ "id" ::: 'Def :=> 'NotNull 'PGint4
   , "label" ::: 'NoDef :=> 'NotNull 'PGtext
   , "description" ::: 'NoDef :=> 'Null 'PGtext
   ]
type ProductConstraints = '["pk_product" ::: 'PrimaryKey '["id"]]
type CategoryColumns =
  '[ "id" ::: 'Def :=> 'NotNull 'PGint4
   , "label" ::: 'NoDef :=> 'NotNull 'PGtext
   ]
type CategoryConstraints = '["pk_category" ::: 'PrimaryKey '["id"]]
type ProductCategoryColumns =
  '[ "product_id" ::: 'NoDef :=> 'NotNull 'PGint4
   , "category_id" ::: 'NoDef :=> 'NotNull 'PGint4
   ]
type ProductCategoryConstraints =
  '[ "pk_product_category_id" ::: 'PrimaryKey '["product_id", "category_id"]
   , "fk_product_id" ::: 'ForeignKey '["product_id"] "public" "product" '["id"]
   , "fk_category_id" ::: 'ForeignKey '["category_id"] "public" "category" '["id"]
   ]
type WarehouseColumns =
  '[ "id" ::: 'Def :=> 'NotNull 'PGint4
   , "product_id" ::: 'NoDef :=> 'NotNull 'PGint4
   , "quantity" ::: 'NoDef :=> 'NotNull 'PGint4
   , "created" ::: 'NoDef :=> 'NotNull 'PGtimestamptz
   , "modified" ::: 'NoDef :=> 'NotNull 'PGtimestamptz
   ]
type WarehouseConstraints =
  '[ "pk_warehouse" ::: 'PrimaryKey '["id"]
   , "fk_product_id" ::: 'ForeignKey '["product_id"] "public" "product" '["id"]
   ]
type Schema =
  '[ "product" ::: 'Table (ProductConstraints :=> ProductColumns)
   , "category" ::: 'Table (CategoryConstraints :=> CategoryColumns)
   , "product_category" ::: 'Table (ProductCategoryConstraints :=> ProductCategoryColumns)
   , "warehouse" ::: 'Table (WarehouseConstraints :=> WarehouseColumns)
   ]
type DB = Public Schema

runThis :: IO ()
runThis = do
  putStrLn "\nRunning squeal"
  withConnection
    Hardcoded.connectionString
    $ cleanUp
    & pqThen insertStuff
    & pqThen queryData
    & pqThen insertWithTransaction
    & pqThen queryWithJoins
    & pqThen errors
  putStrLn "Done"

cleanUp :: PQ DB DB IO ()
cleanUp =
  execute_ teardown
 where
  teardown :: Statement db () ()
  teardown = manipulation $ UnsafeManipulation "truncate warehouse, product_category, product, category"

insertStuff :: (MonadPQ DB m, MonadIO m) => m ()
insertStuff = do
  executePrepared_
    insertProduct
    [ BasicProduct "Wood Screw Kit 1" (Just "245-pieces")
    , BasicProduct "Wood Screw Kit 2" Nothing
    ]
  liftIO $ putStrLn $ "Inserted products."

  result :: [Result (Only Int32)] <-
    executePrepared insertCategory [Category "Screws", Category "Wood Screws", Category "Concrete Screws"]
  rows <- traverse getRows result
  liftIO $ putStrLn $ "Inserted categories: " <> show rows
 where
  insertProduct :: Statement DB BasicProduct ()
  insertProduct =
    manipulation
      $ insertInto_
        #product
        (Values_ (Default `as` #id :* Set (param @1) `as` #label :* Set (param @2) `as` #description))

  insertCategory :: Statement DB Category (Only Int32)
  insertCategory =
    manipulation
      $ insertInto
        #category
        (Values_ (Default `as` #id :* Set (param @1) `as` #label))
        OnConflictDoRaise
        (Returning_ (#id `as` #fromOnly))

queryData :: PQ DB DB IO ()
queryData = do
  result1 <- execute query1
  rows1 <- getRows result1
  liftIO $ putStrLn $ "Query 1: " <> show rows1

  result2 <- executeParams query2 (Only "Wood Screw Kit 1") >>= getRows
  liftIO $ putStrLn $ "Query 2: " <> show result2

  (result3 :: [BasicProduct]) <- execute (query3 ["Wood Screw Kit 2", "Wood Screw Kit 3"]) >>= getRows
  liftIO $ putStrLn $ "Query 3: " <> show result3
 where
  query1 :: Statement DB () BasicProduct
  query1 =
    query
      $ select_
        (#product ! #label :* #product ! #description)
        (from (table #product))

  query2 :: Statement DB (Only Text) BasicProduct
  query2 =
    query
      $ select_
        (#product ! #label :* #product ! #description)
        (from (table #product) & where_ (#product ! #label .== (param @1)))

  query3 labels =
    query
      $ select_
        (#product ! #label :* #product ! #description)
        (from (table #product) & where_ (#product ! #label `in_` labels))

insertWithTransaction :: PQ DB DB IO ()
insertWithTransaction =
  transactionally_
    ( do
        result1 <- executePrepared insertProduct [BasicProduct "Drywall Screws Set" (Just "8000pcs")]
        productIds <- join <$> traverse getRows result1

        result2 <- executePrepared insertCategory [Category "Drywall Screws"]
        categoryIds <- join <$> traverse getRows result2

        case (productIds, categoryIds) of
          ([Only productId], [Only categoryId]) -> do
            executePrepared_ insertProductCategory [(productId, categoryId)]
            executePrepared_ insertListing [(productId, 10)]
          _ ->
            throwM $ userError "Failed to insert product/category"
    )
    >> liftIO (putStrLn $ "Insert with transaction")
 where
  insertProduct :: Statement DB BasicProduct (Only Int32)
  insertProduct =
    manipulation
      $ insertInto
        #product
        (Values_ (Default `as` #id :* Set (param @1) `as` #label :* Set (param @2) `as` #description))
        OnConflictDoRaise
        (Returning_ (#id `as` #fromOnly))

  insertCategory :: Statement DB Category (Only Int32)
  insertCategory =
    manipulation
      $ insertInto
        #category
        (Values_ (Default `as` #id :* Set (param @1) `as` #label))
        OnConflictDoRaise
        (Returning_ (#id `as` #fromOnly))

  insertProductCategory :: Statement DB (Int32, Int32) ()
  insertProductCategory =
    manipulation
      $ insertInto_
        #product_category
        (Values_ (Set (param @1) `as` #product_id :* Set (param @2) `as` #category_id))

  insertListing :: Statement DB (Int32, Int32) ()
  insertListing =
    manipulation
      $ insertInto_
        #warehouse
        (Values_ (Default `as` #id :* Set (param @1) `as` #product_id :* Set (param @2) `as` #quantity :* Set now `as` #created :* Set now `as` #modified))

queryWithJoins :: PQ DB DB IO ()
queryWithJoins = do
  -- printSQL query1
  result <- executeParams query1 (Only 3) >>= getRows
  liftIO $ putStrLn $ "Query with join: " <> show result
 where
  query1 :: Statement DB (Only Int32) Listing
  query1 =
    query
      $ select_
        (#w ! #quantity `as` #quantity :* #p ! #label `as` #label :* #p ! #description `as` #description :* #c ! #label `as` #category)
        ( from
            ( table (#warehouse `as` #w)
                & innerJoin
                  (table (#product `as` #p))
                  (#w ! #product_id .== #p ! #id)
                & leftOuterJoin
                  (table (#product_category `as` #pc))
                  (#pc ! #product_id .== #p ! #id)
                & leftOuterJoin
                  (table (#category `as` #c))
                  (#c ! #id .== #pc ! #category_id)
            )
            & where_ (#w ! #quantity .> (param @1))
        )

errors :: PQ DB DB IO ()
errors = do
  insertDuplicateScrew
  insertDuplicateScrew
    `catchSqueal` (\err -> liftIO $ putStrLn $ "Caught Squeal/SQL Error: " <> displayException err)
 where
  insertDuplicateScrew = executePrepared_ insertProduct [BasicProduct "Duplicate screw" Nothing]
  insertProduct =
    manipulation
      $ insertInto_
        #product
        (Values_ (Default `as` #id :* Set (param @1) `as` #label :* Set (param @2) `as` #description))

data BasicProduct = BasicProduct {label :: Text, description :: Maybe Text}
  deriving stock (Show, GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

newtype Category = Category {label :: Text}
  deriving stock (Show, GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

data Listing = Listing {quantity :: Int32, label :: Text, description :: Maybe Text, category :: Maybe Text}
  deriving stock (Show, GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
