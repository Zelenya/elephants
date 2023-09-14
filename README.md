# How to use PostgreSQL with Haskell. Elephantine Library Review 2023

As of October 2023, there are around a dozen mature PostgreSQL libraries, all levels of abstractions, from low-level queries to the type level. More than enough for anybody. *(Shout out to everyone who says Haskell has no libraries.)* 

Which one to use? Let’s see. Foreach library, we’ll talk about features and basics like writing queries, complexity, pitfalls, (everyone’s favorite topic) errors, and so on.

*We assume you are familiar with the fundamentals of SQL and PostgreSQL.*

## Mise en place

Before integrating with the database, let’s discuss the data and the setup.

> 💡 If you want to follow allow at home, the [repository](https://github.com/Zelenya/elephants ) contains all the imports and data types — we omit most of them from the tutorial for simplicity.

### Data

Let’s imagine that we’re building a (tiny) warehouse management system: 

- **Warehouse** has multiple **products**, some quantity of each.
- **Product** has a label and a description.
- **Product** can belong to **categories** (*many-to-many*).
- **Category** has a label.

The `scripts/create_tables.sql` contains all the definitions:

```sql
CREATE TABLE product (
    id SERIAL PRIMARY KEY,
    label TEXT NOT NULL,
    description TEXT,
    UNIQUE (label)
);

CREATE TABLE category (
    id SERIAL PRIMARY KEY,
    label TEXT NOT NULL,
    UNIQUE (label)
);

CREATE TABLE product_category (
    category_id INT NOT NULL,
    product_id INT NOT NULL,
    PRIMARY KEY (category_id, product_id),
    FOREIGN KEY (product_id) REFERENCES product(id),
    FOREIGN KEY (category_id) REFERENCES category(id)
);

CREATE TABLE warehouse (
    id SERIAL PRIMARY KEY, 
    product_id INT NOT NULL,
    quantity INT NOT NULL,
    created TIMESTAMP,
    modified TIMESTAMP,
    FOREIGN KEY (product_id) REFERENCES product(id)
);
```

![schema](https://github.com/Zelenya/elephants/assets/11508062/92f8e097-a5c5-427f-b848-76d6edc23e7b)

### Postgres server

Here are a few things you need to know to play along at home. 

> 🐘 If you don’t care, don’t like docker, or already have an easily-accessible postgres server, feel free to skip this section.

First, install docker. 

`docker compose up` (`docker-compose up` on older versions) starts [PostgreSQL](https://hub.docker.com/_/postgres) (see `docker-compose.yml`) and initializes the databases and tables (using the `scripts/create_tables.sql`). It mounts the data to the `postgres-data/` (in case you need to wipe it or something).

Some of the hardcoded things that the code relies on:

```docker
environment:
  - POSTGRES_DB=warehouse
  - POSTGRES_USER=postgres
  - POSTGRES_PASSWORD=password
ports:
  - 5432:5432
```

> 💡 These hardcoded values are also hardcoded in the code in `Hardcoded.hs` (for laziness reasons)

You can connect to the container and run arbitrary queries using `psql`: 

```bash
docker exec -it elephants-postgres-1 psql -U postgres -d warehouse
```

**Note:** `elephants-postgres-1` is a container name, which might be different for you; check with `docker ps` to get the correct container id (or name). We also pass a couple of flags: `-U postgres` for the user name and `-d warehouse` for the database name.

`docker compose down` to stop and remove the containers.

### Project Overview

If you have `stack` [installed](https://www.haskell.org/ghcup/install/#how-to-install), `stack build` to build and `stack run` to run. 

> 💡 We use [lts-21.7](https://www.stackage.org/lts-21.7) (ghc-9.4.5), published on 2023-08-14.
>

> 🤷 To build the backend, you might need the `libpq` development libraries installed (e.g., `libpq-dev` on Debian-based distributions).
> 
### Extensions overview

Note that most of the libraries rely on using various extensions. Here is a quick overview of the most important ones.

---

`OverloadedStrings`

Used to simplify the construction of query values — we can use literal strings, like `"SELECT * FROM user"`, instead of manually constructing the whole type; for example, `Query . toByteString . stringUtf8 $ "SELECT * FROM user"` (see [Query](https://hackage.haskell.org/package/postgresql-simple-0.7.0.0/docs/Database-PostgreSQL-Simple.html#t:Query) in `postgresql-simple`).

---

`TemplateHaskell`

Template Haskell (TH) is fairly useful for generating boilerplate code. Some libraries provide the TH machinery to derive typeclass instances and/or generate custom type-safe data types at the compile time.

---

`DeriveAnyClass` and `DeriveGeneric` enable alternative ways to get free typeclass instances.

We’ll use `DerivingStrategies` to make the derivation explicit, for example:

```haskell
data Category = Category {label :: Text}
  deriving (Show, Generic)           -- Derive Generic instance,
  deriving anyclass (ToRow, FromRow) -- used for these instances
```

---

`QuasiQuotes`

Some libraries (e.g., `postgresql-simple`) provide quasi quoters for less tedious sql construction in Haskell code. For example:

```haskell
[sql| select label from product |]
```

---

`DuplicateRecordFields`

Only one postgres library requires it. But it’s a stylistic choice — we use this extension because we have multiple data types with a `label` field.

---

`OverloadedRecordDot`

To access record fields as well as specific columns of the tables (for example, `product.label`)

---

Note that the project uses [`GHC2021`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/control.html#extension-GHC2021), which includes [`DeriveGeneric`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/generics.html#extension-DeriveGeneric), [`TypeApplications`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_applications.html#extension-TypeApplications), and many other extensions required for some libraries to work. Also, for the tutorial's sake, we’ll use the included `ScopedTypeVariables` to demonstrate some intermediate types.

## `postgresql-simple`

Let’s start simple. [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple) describes itself as *“Mid-Level PostgreSQL client library“*.

In other words, we’ll write raw sql queries, and the library will deal with security and stuff.

To get started, we add `postgresql-simple` to dependencies. We’re using `v0.7.0.0` published in 2023.

### How to connect to a database

We use `connect` to acquire a connection. It accepts `ConnectInfo`, which we can get by using `defaultConnectInfo` and overriding some defaults.

```haskell
getConnection :: IO Connection
getConnection =
  connect $
    defaultConnectInfo
      { connectHost = Hardcoded.host
      , connectDatabase = Hardcoded.database
      , connectUser = Hardcoded.user
      , connectPassword = Hardcoded.password
      }
```

Eventually, we have to `close connection`. But you will probably not need to do it manually because you can use [`withConnect`](https://hackage.haskell.org/package/postgresql-simple-0.7.0.0/docs/Database-PostgreSQL-Simple.html#v:withConnect), [`bracket`](https://hackage.haskell.org/package/base-4.18.0.0/docs/Control-Exception.html#v:bracket), or (better) a **connection pool**.

The library doesn’t support pools, but you can use the [resource-pool](https://hackage.haskell.org/package/resource-pool) package (or something similar).

### How to modify data

We use [`execute`](https://hackage.haskell.org/package/postgresql-simple-0.7.0.0/docs/Database-PostgreSQL-Simple.html#v:execute) and [`execute_`](https://hackage.haskell.org/package/postgresql-simple-0.7.0.0/docs/Database-PostgreSQL-Simple.html#v:execute_) to insert, update, and delete data.

The version with the `_` suffix is simpler — it doesn’t perform any query substitutions. We can use it with hardcoded values or with straightforward queries such as truncating the tables:

```haskell
cleanUp :: Connection -> IO ()
cleanUp connection =
  void $ execute_ connection "truncate warehouse, product_category, product, category"
```

Both *execute* functions return the number of affected rows, which isn’t relevant in case of truncate (it’s `0`) and safe to ignore ([`void`](https://hackage.haskell.org/package/base-4.18.0.0/docs/Control-Monad.html#v:void) ignores the result of evaluation).

We can use `execute` to make a proper insert and pass some values for substitutions. The simplest way is to pass a tuple:

```haskell
insert1 <-
  execute
    connection
    "insert into product (label, description) values (?, ?)"
    ("Wood Screw Kit 1" :: Text, "245-pieces" :: Text)
```

Sometimes, we must be explicit about types; for example, in cases like this, when we use string literals with `OverloadedStrings` or numeric literals (like `245`).

Because there is no tuple of 1, the library provides a custom type `Only`:

```haskell
insert2 <-
  execute
    connection
    "insert into product (label) values (?)"
    (Only "Wood Screw Kit 2" :: Only Text)
```

Alternatively, we can use lists for any number of values:

```haskell
insert3 <-
  execute
    connection
    "insert into product (label) values (?)"
    ["Wood Screw Kit 3" :: Text]
```

But preferable, we use dedicated types:

```haskell
execute
  connection
  "insert into product (label, description) values (?, ?)"
  (BasicProduct "Wood Screw Kit 4" (Just "245-pieces"))
```

A record can be turned into a list of substitutions via the [`ToRow`](https://hackage.haskell.org/package/postgresql-simple-0.7.0.0/docs/Database-PostgreSQL-Simple.html#t:ToRow) typeclass, which is derivable using GHC generics:

```haskell
data BasicProduct = BasicProduct {label :: Text, description :: Maybe Text}
  deriving (Show, Generic)
  deriving anyclass (ToRow, FromRow)
```

If we want to modify multiple rows, we can use [`executeMany`](https://hackage.haskell.org/package/postgresql-simple-0.7.0.0/docs/Database-PostgreSQL-Simple.html#v:executeMany):

```haskell
insert5 <-
 executeMany
   connection
   insert into category (label) values (?)"
   [Category "Screws", Category "Wood Screws", Category "Concrete Screws"]
```

### How to query data

The execute functions can’t return any results (other than the number of affected rows), so we have to use the query functions.

Similar to `execute_`, [`query_`](https://hackage.haskell.org/package/postgresql-simple-0.7.0.0/docs/Database-PostgreSQL-Simple.html#v:query_) takes a query with no substitutes:

```haskell
query1 :: [(Int64, Text, Maybe Text)] <-
  query_ connection "select id, label, description from product"
```

Note that we must be explicit about return types — the library can’t guess what we want. In this case, we expect an id of type `Int64` that corresponds to `Serial`, required `Text` label, and optional description.

We can specify a record return type if we derive [`FromRow`](https://hackage.haskell.org/package/postgresql-simple-0.7.0.0/docs/Database-PostgreSQL-Simple.html#t:FromRow) (recall `ToRow` from the previous section). For example, let’s get a `BasicProduct` list by label using `query`:

```haskell
query2 :: [BasicProduct] <-
  query 
    connection 
    "select label, description from product where label = ? "
    (Only "Wood Screw Kit 2" :: Only Text)
```

If we want to use the in-clause, the library provides a dedicated wrapper:

```haskell
query3 :: [BasicProduct] <-
  query connection "select label, description from product where label in ?" $
    Only (In ["Wood Screw Kit 2" :: Text, "Wood Screw Kit 3"])
```

### How to use transactions

Imagine we want to atomically insert a new listing: product, category, and quantity. This touches multiple tables and requires a transaction. Additionally, because we have a many-to-many relationship, we must first insert the product and category and then use their new ids to create a mapping.

We can use `returning` to get `id`s of created rows:

```haskell
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
```

Note that we must use `query` and not `execute` because these queries return results.

We can use [`withTransaction`](https://hackage.haskell.org/package/postgresql-simple-0.7.0.0/docs/Database-PostgreSQL-Simple.html#v:withTransaction) to wrap multiple queries in a transaction: 

```haskell
withTransaction connection $ do
  productIds :: [Only Int64] <- query ...
  categoryIds :: [Only Int64] <- query ...

  void $ case (productIds, categoryIds) of
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
```

Any error will rollback the transaction (and the exception will be rethrown). In this example, we throw an explicit error if we don’t get the expected ids for some reason.

Note that in case of a **sql error**, the exception will not only rollback the transaction but, if uncaught, will propagate further (killing everything on its way and potentially crashing the whole app). So, we should (at least) wrap transactions in the exception handler(s); we’ll see how to do this later.

When you need to, you can also use granular transaction functions: [begin](https://hackage.haskell.org/package/postgresql-simple-0.7.0.0/docs/Database-PostgreSQL-Simple.html#v:begin), [commit](https://hackage.haskell.org/package/postgresql-simple-0.7.0.0/docs/Database-PostgreSQL-Simple.html#v:commit), and [rollback](https://hackage.haskell.org/package/postgresql-simple-0.7.0.0/docs/Database-PostgreSQL-Simple.html#v:rollback).

### How to query using joins

To read all these tables at once, we need to query using a few joins. The library provides a quasi-quoter that makes writing big queries easier — we can format the query and not worry about whitespaces:

```haskell
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
```

### Errors

In `postgresql-simple`, all the programmer errors (in sql or library usage) are (runtime) exceptions.

If the query string is not formatted correctly, we get [`FormatError`](https://hackage.haskell.org/package/postgresql-simple-0.7.0.0/docs/Database-PostgreSQL-Simple.html#t:FormatError). For instance, if we have a mismatching number of substitutions (`?` and actual values):

```haskell
execute
  connection
  "INSERT INTO category (label) VALUES (?)"
  ("One" :: Text, "Two" :: Text)
```

> `FormatError {fmtMessage = "1 single '?' characters, but 2 parameters", fmtQuery = "INSERT INTO category (label) VALUES (?)", fmtParams = ["One","Two"]}`
> 

Similarly, on the return side, if the number of columns doesn’t match the number of elements in the result type (in a list, a tuple, or a record), we get [`ResultError`](https://hackage.haskell.org/package/postgresql-simple-0.7.0.0/docs/Database-PostgreSQL-Simple.html#t:ResultError). The most likely variants are `Incompatible` and `UnexpectedNull`.

If we forget to wrap a nullable type on the Haskell side, we get `UnexpectedNull`. For instance, if we try to get `description` (which is nullable) as `Text` and not `Maybe Text`:

```haskell
let result :: IO [(Text, Text)] = query_ connection "select label, description from product"
```

> `UnexpectedNull {errSQLType = "text", errSQLTableOid = Just (Oid 16386), errSQLField = "description", errHaskellType = "Text", errMessage = ""}`
> 

If we mistype the types, we get `Incompatible`. For instance, if we try to parse just `id` into `BasicProduct`:

```haskell
let result :: IO [BasicProduct] = query_ connection "select id from product"
```

> `Incompatible {errSQLType = "int4", errSQLTableOid = Just (Oid 16386), errSQLField = "id", errHaskellType = "Text", errMessage = "types incompatible"}`
> 

On top of that, if we misuse the library — by mistaking `query` for `execute` or vice verse — we get [`QueryError`](https://hackage.haskell.org/package/postgresql-simple-0.7.0.0/docs/Database-PostgreSQL-Simple.html#t:QueryError). For example, if we use `execute` with *insert* query that has *returning*:

```haskell
execute_
  connection
  "INSERT INTO category (label) VALUES (Screws) returning id"
```

> `QueryError {qeMessage = "execute resulted in Col 1-column result", qeQuery = "INSERT INTO category (label) VALUES ('Deck Screws') returning id"}`
> 

And last but not least, any sql errors from postgres, will come back as [`SqlError`](https://hackage.haskell.org/package/postgresql-simple-0.7.0.0/docs/Database-PostgreSQL-Simple.html#t:SqlError):

```haskell
let result :: IO [BasicProduct] = query_ connection "select I have no idea what I'm doing"
```

> `Wrong sql: SqlError {sqlState = "42601", sqlExecStatus = FatalError, sqlErrorMsg = "syntax error at or near \"no\"", sqlErrorDetail = "", sqlErrorHint = ""}`
> 

The errors are pretty good but still not the most descriptive — if you try to write big queries, you have to concentrate on projecting the error information to the query.

### Resources

The docs are also simple; the library covers all the primary blocks, describes the functions, and provides some examples. Outside, a few blog posts cover similar things, mainly targeting beginners.

And you don’t need more than that — if you know how to write one simple query, you know how to write them all.

### Migrations

The library has a companion [`postgresql-simple-migration`](https://hackage.haskell.org/package/postgresql-simple-migration), which has been archived in has been archived in 2021. What does it mean? *I don’t know.*

### In summary

[postgresql-simple](https://hackage.haskell.org/package/postgresql-simple) is a library for all levels and a great option if you love writing sql by hand and don’t need reusability.

It doesn’t parse or validate the queries, so we must pay attention to what we write: sql queries, haskell types (type-safety is our responsibility), the order of parameters, and so on.

## `hasql`

The next “obvious” step is to add more type-safety.

According to the readme, *[Hasql](https://github.com/nikita-volkov/hasql) “is a highly efficient PostgreSQL driver for Haskell with a typesafe yet flexible mapping API; it is production-ready, actively maintained, and the API is pretty stable. It's used by many companies and most notably by the [Postgrest](https://postgrest.org/) project.“*

Hasql is an [ecosystem](https://github.com/nikita-volkov/hasql#ecosystem) of libraries. To keep it simple, let’s limit ourselves to core `hasql`, [`hasql-transaction`](https://github.com/nikita-volkov/hasql-transaction), and [`hasql-th`](https://github.com/nikita-volkov/hasql-th). We’re using `hasql` `1.6.3.2` published in 2023.

We’ll also use [contravariant-extras](https://hackage.haskell.org/package/contravariant-extras), [vector](https://hackage.haskell.org/package/vector), [profunctors](https://hackage.haskell.org/package/profunctors), and [tuple](https://hackage.haskell.org/package/tuple) packages to make a few things tidier (this isn’t required; it’s all copy-paste anyway).

> 💡 (It’s not very important, but) We assume you’ve seen the part on `postgresql-simple`, which covers the same topics but at a slower pace. 
### How to connect to a database

First, we get a connection:

```haskell
Right connection <- getConnection
```

```haskell
getConnection :: IO (Either ConnectionError Connection)
getConnection =
  acquire $ settings Hardcoded.host Hardcoded.portNumber Hardcoded.user Hardcoded.password Hardcoded.database
```

Note the `Either`. But for now, let’s just pattern-match and not worry about possible errors…

In reality/production, we should probably use [`hasql-pool`](https://hackage.haskell.org/package/hasql-pool) to work with a pool of connections.

### How to modify data

Let’s see the leading players through the clean-up query:

```haskell
cleanUp :: Connection -> IO (Either QueryError ())
cleanUp connection = run cleanUpSession connection
 where
  cleanUpSession :: Session ()
  cleanUpSession = statement () cleanUpStatement

  cleanUpStatement :: Statement () ()
  cleanUpStatement = Statement rawSql E.noParams D.noResult True

  rawSql = "truncate warehouse, product_category, product, category"
```

- `Session` is a batch of actions to be executed in the context of a connection (a query).
- `Statement` is a specification of a strictly single-statement query, which can be parameterized and prepared (how to make a query).
- `Statement` consists of SQL template, params encoder, result decoder, and a flag that determines whether it’s prepared.
- `statement` creates a `Session` from a `Statement` and input parameters.
- `run` executes a bunch of commands (statements) on the provided connection.

> 💡 Remember that you can see the complete code in the [repo](https://github.com/Zelenya/elephants).

We have a simple query with no parameters and no result — we don’t need to encode or decode anything. That’s what `E.noParams D.noResult` for. If we want to pass parameters, we need to supply a decoder.

The first option, is tuples of primitive types and manually written decoders:

```haskell
insertProductSql = "insert into product (label, description) values ($1, $2)"
```

```haskell
insertProduct1 :: Statement (Text, Maybe Text) Int64
insertProduct1 = Statement insertProductSql rawParams D.rowsAffected True

rawParams =
  (fst >$< E.param (E.nonNullable E.text))
    <> (snd >$< E.param (E.nullable E.text))
```

```haskell
statement ("Wood Screw Kit 1", Just "245-pieces") insertProduct1
```

`rawParams` is the encoder for our parameters. We use [`contramap`](https://hackage.haskell.org/package/base-4.18.0.0/docs/Data-Functor-Contravariant.html#v:contramap) operator (`>$<`) and append (`<>`) to compose multiple parameters. `D.rowsAffected` is the decoder for the result when we want to return the number of affected rows.

> 💡 Instead of [`fst`](https://hackage.haskell.org/package/base-4.16.3.0/docs/Data-Tuple.html#v:fst) and [`snd`](https://hackage.haskell.org/package/base-4.16.3.0/docs/Data-Tuple.html#v:snd), you can use the `contrazip` family of functions from the `contravariant-extras` package to reduce boilerplate.

Another option, is using records:

```haskell
insertProduct2 :: Statement BasicProduct Int64
insertProduct2 = Statement insertProductSql basicProductParams D.rowsAffected True

basicProductParams :: E.Params BasicProduct
basicProductParams =
  ((.label) >$< E.param (E.nonNullable E.text))
    <> ((.description) >$< E.param (E.nullable E.text))
```

```haskell
statement (BasicProduct "Wood Screw Kit 2" Nothing) insertProduct2
```

If we want to modify multiple rows, we have to use the postgres `unnest` function:

```haskell
insertManyCategories :: Statement (Vector Category) Int64
insertManyCategories = Statement insertManyCategoriesSql categoryParams D.rowsAffected True

insertManyCategoriesSql = "insert into category (label) select * from unnest ($1)"

categoryParams :: E.Params (Vector Category)
categoryParams =
  E.param
    $ E.nonNullable
    $ E.array
    $ E.dimension List.foldl'
    $ categoryArray

categoryArray :: E.Array Category
categoryArray = (.label) >$< (E.element $ E.nonNullable E.text)
```

`categoryParams` is an encoder that allows us to pass a vector of categories to insert. 

```haskell
let categories = [Category "Screws", Category "Wood Screws", Category "Concrete Screws"]
statement (fromList categories) insertManyCategories
```

> Note that `unnest` is more efficient than executing a single-row insert statement multiple times.

### How to query data

Querying data is similar:

```haskell
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
```

We need to provide a decoder for the result (to specify how each row results maps into the expected type). If this sounds tedious, we can ask Template Haskell to do the work for us:

In this case, we use `singletonStatement` that expects one result. There are other variants that we’ll see later.

```haskell
session2 :: Session (Text, Maybe Text)
session2 = statement () statement2

statement2 :: Statement () (Text, Maybe Text)
statement2 =
  [singletonStatement|
    select label :: text, description :: text? from product limit 1
  |]
```

We write the query and specify the types, `hasql-th` handles the codecs for us.

But we still need to handle the conversions if we use custom types instead of tuples. The result of the statement has a `Profunctor` instance, which allows us to modify (input) parameters and (output) results. In other words, we use `lmap` to map parameters, `rmap` — result, and `dimap` — both. For example, let’s return `BasicProduct`:

```haskell
session3 :: Session (Maybe BasicProduct)
session3 = statement "Wood Screw Kit 2" statement3

statement3 :: Statement Text (Maybe BasicProduct)
statement3 =
  rmap
    (fmap (uncurryN BasicProduct))
    [maybeStatement|
      select label :: text, description :: text?
      from product
      where label = $1 :: text
    |]
```

---

💡 `(fmap (uncurryN BasicProduct))` is a concise way to write the following (using `tuples` package):

```haskell
(\result -> fmap (\(a, b) -> (BasicProduct a b)) result)
```

---

`hasql` doesn’t have "special support" for an array as a parameter for the `IN` operator, we should use [Any](https://hackage.haskell.org/package/hasql-1.6.3.2/docs/Hasql-Statement.html#g:3):

```haskell
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
```

### How to use transactions

We can use `returning` to get `id`s of created rows:

```haskell
insertProduct :: Statement (Text, Maybe Text) Int64
insertProduct =
  [singletonStatement|
    insert into product (label, description) values ($1 :: text, $2 :: text?) returning id :: int8
  |]
```

To wrap multiple queries in a transaction, we can use `hasql-transaction`. First, we compose the statements:

```haskell
insertAll :: FullProduct -> Transaction Int64
insertAll listing = do
  productId <- Transaction.statement (listing.label, listing.description) insertProduct
  categoryId <- Transaction.statement listing.category insertCategory
  _ <- Transaction.statement (productId) insertListing
  ids <- Transaction.statement (productId, categoryId) insertMapping
  pure ids

insertProduct :: Statement (Text, Maybe Text) Int64

insertCategory :: Statement Text Int64

insertListing :: Statement Int64 ()

insertMapping :: Statement (Int64, Int64) Int64
```

Then we run the transaction using the relevant isolation level and mode:

```haskell
insertWithTransaction :: Connection -> IO ()
insertWithTransaction connection = do
  let listing = FullProduct "Drywall Screws Set" (Just "8000pcs") "Drywall Screws"
  mapping <- run (transaction Serializable Write $ insertAll listing) connection
  putStrLn $ "Insert with transaction: " <> show mapping
```

### How to query using joins

We can query these tables using a few joins. There should be nothing unexpected here:

```haskell
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
```

### Errors

We’ve been neglecting this until now, but all error reporting is explicit and is presented using `Either`.

---

💡 Just a reminder, **don’t ignore errors**. And don’t pattern match only on `Right`, or you will end up with this:

```haskell
user error (Pattern match failure in 'do' block at ...)
```

---

The other good thing is that the `hasql-th` parser is pretty good at error reporting and catching typos at compile time (and most of the time, it’s more explicit than postgres’ `syntax error at or near`). This won’t compile:

```haskell
[singletonStatement|
    select I have no idea what I'm doing
|]
```

The library doesn’t accept (doesn’t compile) if you forget to specify one of the types. For instance, if we omit type of `label`, we get a somewhat generic error:

```haskell
[singletonStatement|
    select label, description :: text? from product
|]
```

> `Result expression is missing a typecast`
> 

This ensures that most input and result type (including nullability) mismatches are caught in the compile time. For example, if we forget an input type and return a wrong result type:

```haskell
statement :: Statement () (Text, Int32)
statement =
  [singletonStatement|
    select label :: text, description :: text?
--                                       ^^^^^ 
--  Couldn't match type ‘Int32’ with ‘Maybe Text’
    from product where label = $1 :: text
--                                   ^^^^
--  Couldn't match type ‘Text’ with ‘()’
  |]
```

However, we’re not safe from programming errors. We should use correct statement functions not to get a runtime error. For example, if we use `singletonStatement` for statements that might not return a result (instead of `maybeStatement`):

```haskell
do
  failure <- run (statement () failsBecauseNoResults) connection
  putStrLn $ "Wrong statement function: " <> show failure
 where
  failsBecauseNoResults :: Statement () (Text)
  failsBecauseNoResults =
    [singletonStatement|
        select label :: text from product where 1 = 0
    |]
```

> `Wrong statement function: Left (QueryError "SELECT label :: text FROM product WHERE 1 = 0" [] (ResultError (UnexpectedAmountOfRows 0)))`
> 

Or if we use `singletonStatement` with `()` result (instead of `resultlessStatement`):

```haskell
do
  failure <- run (statement () failsBecauseResultless) connection
  putStrLn $ "Wrong statement function: " <> show failure
 where
  failsBecauseResultless :: Statement () ()
  failsBecauseResultless =
    [singletonStatement|
      insert into product (label) values ('this insert fails')
    |]
```

> `Wrong statement function: Left (QueryError "INSERT INTO product (label) VALUES ('this insert fails')" [] (ResultError (UnexpectedResult "Unexpected result status: CommandOk")))`
> 

In case of runtime sql error, for instance, if we violate a constraint, we get a similar error:

```haskell
inserProduct :: Statement Text ()
inserProduct =
  [singletonStatement|
    insert into product (label) values ($1 :: text)
  |]
```

```haskell
run (statement "Duplicate screw" inserProduct) connection
  >> run (statement "Duplicate screw" inserProduct) connection
```

> `Wrong statement function (Left): QueryError "INSERT INTO product (label) VALUES ('Duplicate')" [] (ResultError (ServerError "23505" "duplicate key value violates unique constraint \"product_label_key\"" (Just "Key (label)=(Duplicate screw) already exists.") Nothing Nothing))`
> 

### Resources

Core readme has a good overview and example. The library has simple docs, a couple of tutorials, and talks from the author.

### Migrations

[hasql-migrations](https://github.com/tvh/hasql-migration) tool is a port of `postgresql-simple-migration` for use with `hasql`.

### In summary

Overall, `hasql` is a great choice for writing raw sql queries with more type safety and compile-time syntax checks. The ecosystem comes with other whistles like connection pools and transactions. 

The TemplateHaskell module and compile-time checks are optional — if you want, you can deal with the encoders and decoders yourself.

The library requires basic/intermediate knowledge of Haskell and ecosystems. To be comfortable and productive, you must be familiar with vectors, contravariant functors, etc. Other than that, the library is relatively straightforward.

## `persistent` + `esqueleto`

*If that was not enough, it’s time to move to the type level.*

According to the readme, *”[Persistent's](https://github.com/yesodweb/persistent) goal is to catch every possible error at compile-time, and it comes close to that.* It is also *designed to be adaptable to any datastore”*. As a result, *”a major limitation for SQL databases is that the persistent library does not directly provide joins”*.

However, we can use [Esqueleto](http://hackage.haskell.org/package/esqueleto) (*”a bare bones, type-safe EDSL for SQL queries”*) with Persistent's serialization to write type-safe SQL queries. It’s unlikely that you want to use Persistent by itself with SQL, so let’s use and review them together.

We’re using [`persistent`](https://hackage.haskell.org/package/persistent) (`2.14.5.1`), [`persistent-postgresql`](https://hackage.haskell.org/package/persistent-postgresql) (`2.13.5.2`), and [`esqueleto`](https://hackage.haskell.org/package/esqueleto) (`3.5.10.1`), all published in 2023. Additionally, we’ll use the **experimental** style, which will become the new "default" in `esqueleto-4.0.0.0`.

We’ll also use `mtl`, `monad-logger`, `unliftio-core`, `time`, and `exceptions`.

The libraries require additional extensions: `DataKinds`, `GADTs`, `TypeFamilies`, and `UndecidableInstances`.

> 💡 (It’s not very important, but) We assume you’ve seen the part on `postgresql-simple`, which covers the same topics but at a slower pace.

### How to connect to a database

`Database.Persist.Postgresql` provides various ways to connect to postgres with and without a connection pool.

First, we need a `libpq` connection string, which looks like this `"host=localhost port=5432 user=postgres dbname=warehouse password=password"`.

We create the pool and run actions on it using `withPostgresqlPool` and passing the connection string, number of connections, and action(s) to be executed. We use `liftSqlPersistMPool` to run an action/transaction on a pool. And “finally”, use `runNoLoggingT` (`runStdoutLoggingT`, or alternative) to run with appropriate logging.

```haskell
runNoLoggingT $ P.withPostgresqlPool Hardcoded.connectionString 3 $ \pool -> do
    le runWithPool = flip liftSqlPersistMPool pool
    runWithPool transaction1
    runWithPool transaction2
    ...
```

> 💡 We can use `runStdoutLoggingT` to see what sql queries get executed.

### How to define tables

Persistent takes care of creating and matching Haskell datatypes and `PersistEntity` instances; we need to declare the entities by passing them to `mkPersist`:

```haskell
mkPersist
  sqlSettings
  [persistLowerCase|
  Product
    label Text
    description Text Maybe
    UniqueLabel label
    deriving Eq Show
  Category
    label Text
    UniqueCategory label
    deriving Eq Show
  ProductCategory
    productId ProductId
    categoryId CategoryId
    Primary productId categoryId
    deriving Eq Show
  Warehouse
    productId ProductId
    quantity Int
    created UTCTime default=CURRENT_TIME
    modified UTCTime default=CURRENT_TIME
    deriving Eq Show
|]
```

`persistLowerCase` states that `SomeTable` corresponds to the SQL table `some_table`.

### How to modify data

Even though it’s not encouraged, we can always execute raw sql; for example, we can truncate tables with [`rawExecute`](https://hackage.haskell.org/package/esqueleto-3.5.10.3/docs/Database-Esqueleto-Experimental.html#v:rawExecute):

```haskell
cleanUp :: (MonadIO m) => SqlPersistT m ()
cleanUp = rawExecute "truncate warehouse, product_category, product, category" []
```

What’s `SqlPersistT m ()`? Let’s say it’s something that can be executed with `runWithPool` and returns `()`.

---

💡 Note that we can also use `deleteWhere` to delete all the records from a table:

```haskell
deleteWhere ([] :: [Filter Product]))
```

---

Because we’ve done all the groundwork, we use records right away (no tuples):

```haskell
insertStuff :: (MonadIO m) => SqlPersistT m ()
insertStuff = do
  newId <- insert $ Product "Wood Screw Kit 1" (Just "245-pieces")
  liftIO $ putStrLn $ "Insert 1: " <> show newId

  newIds <- insertMany [Category "Screws", Category "Wood Screws", Category "Concrete Screws"]
  liftIO $ putStrLn $ "Insert 2: " <> show newIds
```

That’s it! Persistent is concise when it comes to inserts. Note that `insert` returns the id, and `insertMany` returns multiple ids. 

We can use `liftIO` to execute `IO` operations like printing “inside” `SqlPersistT`.

### How to query data

This is the part where `esqueleto` comes in.

The first query takes a label and returns a list of product entities:

```haskell
query1 :: Text -> SqlPersistT m [Entity Product]
query1 label = select $ do
  aProduct <- from $ table @Product
  where_ (aProduct.label ==. val label)
  pure aProduct
```

It returns an `Entity` instead of a value — an `Entity` combines a database id and a value. 

This is an experimental syntax that mimics sql. We use the `TypeApplications` extensions to make the table explicit, `OverloadedRecordDot` to select the field/column value,  the `==.` operator to check for equality, and `val` to “lift” haskell value into “sql query land”.

> 💡 Note that there are other alternatives for field projections (instead of `OverloadedRecordDot`), such as the `(^.)` operator and `OverloadedLabels`.

We can select multiple labels using `in_`:

```haskell
query2 :: [Text] -> SqlPersistT m [Entity Product]
query2 lables = select $ do
  aProduct <- from $ table @Product
  where_ $ aProduct.label `in_` valList lables
  pure aProduct
```

### How to use transactions

We’ve been kind-of using transactions all this time. Everything inside a **single call** to `liftSqlPersistMPool` (and other versions, with and without pooling) runs in a **single transaction**.

```haskell
insertWithTransaction :: (MonadIO m, MonadCatch m) => SqlPersistT m ()
insertWithTransaction = handle (\(SomeException _) -> pure ()) $ do
  productId <- insert $ Product "Drywall Screws Set" (Just "8000pcs")
  categoryId <- insert $ Category "Drywall Screws"
  time <- liftIO getCurrentTime
  _ <- insert_ $ Warehouse productId 10 time time
  _ <- insert_ $ ProductCategory productId categoryId
  liftIO $ putStrLn $ "Insert with transaction"
```

This time, we handle exceptions (any `SomeException`). 

> 💡 We generally want to split the queries into transactions and catch exceptions on each transaction. We dive deeper into error handling in the errors section.

### How to query using joins

And this is the part where experimental syntax comes in handy:

```haskell
query quantity = select $ do
  (warehouse :& aProduct :& _ :& category) <-
    from
      $ table @Warehouse
      `innerJoin` table @Product
      `on` do \(w :& p) -> w.productId ==. p.id
      `LeftOuterJoin` table @ProductCategory
      `on` do \(_ :& p :& pc) -> just p.id ==. pc.productId
      `LeftOuterJoin` table @Category
      `on` do \(_ :& _ :& pc :& c) -> pc.categoryId ==. c.id
  where_ (warehouse.quantity >. val quantity)
  pure $ (warehouse.quantity, aProduct.label, aProduct.description, category.label)
```

The [`on`](https://hackage.haskell.org/package/esqueleto-3.5.10.3/docs/Database-Esqueleto-Experimental.html#v:on) clauses are attached directly to the relevant join. The `ON` clause lambda has all the available tables — only the tables we have already joined into are in scope. 

We use the [`:&`](https://hackage.haskell.org/package/esqueleto-3.5.10.3/docs/Database-Esqueleto-Experimental.html#t::-38-) operator to pattern match against the joined tables. We use `_` placeholder to ignore the previous references to the table.

This generates this query:

```sql
SELECT 
  "warehouse"."quantity", 
  "product"."label", 
  "product"."description", 
  "category"."label" 
FROM 
  "warehouse" 
  INNER JOIN "product" ON "warehouse"."product_id" = "product"."id" 
  LEFT OUTER JOIN "product_category" ON "product"."id" = "product_category"."product_id" 
  LEFT OUTER JOIN "category" ON "product_category"."category_id" = "category"."id" 
WHERE 
  "warehouse"."quantity" > ?
```

### Errors

It’s possible to write type-checked queries that fail at runtime, but most typical sql errors are caught as compile-time errors.

Sometimes, mistakes in queries will result in error messages that refer to library internals (for example, you might see `PersistUniqueRead backend0`, `Database.Esqueleto.Internal.Internal.SqlExpr`, `PersistRecordBackend backend val`, `‘BaseBackend backend0’`, `‘SqlBackend’`). This takes some time to get used to. Help the type inference, and it will help you.

Nobody is safe from runtime sql errors. For example, if we violate the uniqueness constraint, we get an exception that we need to deal with:

```haskell
errors :: (MonadIO m, MonadCatch m) => SqlPersistT m ()
errors = do
  let duplicateScrew = Product "Duplicate screw" Nothing
  void $ insert duplicateScrew
  (void $ insert duplicateScrew)
    `catch` (\(SomeException err) -> liftIO $ putStrLn $ "Caught SQL Error: " <> displayException err)
```

> `Caught SQL Error: SqlError {sqlState = "23505", sqlExecStatus = FatalError, sqlErrorMsg = "duplicate key value violates unique constraint \"product_label_key\"", sqlErrorDetail = "Key (label)=(Duplicate screw) already exists.", sqlErrorHint = ""}`
> 

Note that we use the [exceptions](https://hackage.haskell.org/package/exceptions) package to handle exceptions. (We don’t use the exceptions from [Control.Exception](https://hackage.haskell.org/package/base-4.16.3.0/docs/Control-Exception.html) as we did in `postgresql-simple` because we don’t want to be limited to `IO`).

### Resources

`persistent` is well documented as part of the [yesod book](https://www.yesodweb.com/book/persistent), and `esqueleto` has good readme and docs. The catch is that you have to keep an eye on multiple packages simultaneously. 

On top of that, (currently) `esqueleto` supports legacy and experimental syntax, and you have to be aware that some tutorials and examples use less safe legacy syntax (or a mix of both) — the good news is that the compiler will warn you if you’re on the wrong path. 

### Migrations

`persistent` can automatically create tables and do migrations. However, the [book](https://www.yesodweb.com/book/persistent#persistent_migrations) discourages that: 

> *“Using automated database migrations is only recommended in development environments. Allowing your application to modify your database schema in a production environment is very strongly discouraged.”*
> 

### In summary

You should consider `persistent` with `esqueleto` if you mainly have a lot of simple queries, are tired of writing raw sql, but want moderately more type-safe and composable sql. 

The `persistent` is a (very) generalized library, meaning you should be comfortable using abstractions. And you should be familiar with `mtl`, `monad-logger`, lifting/unlifting IO, and so on.

> *“Most kinds of errors committed when writing SQL are caught as compile-time errors — although it is possible to write type-checked `esqueleto` queries that fail at runtime”*
> 

---

If you look around, some tutorials and comparisons might say that `esqueleto` joins might lead to to runtime errors. Don’t worry — this refers to legacy syntax — use new/experimental syntax.

## `beam`

*Tired of sql and Template Haskell?*

[Beam](https://haskell-beam.github.io/beam/) “*is a highly-general library for accessing any kind of database with Haskell”.* Beam makes extensive use of GHC's Generics mechanism — no Template Haskell.

First, install [`beam-core`](https://hackage.haskell.org/package/beam-core) (`0.10.1.0` released in 2023) and [`beam-postgres`](https://hackage.haskell.org/package/beam-postgres)(`0.5.3.1`). 

A few additional extensions: `GADTs` and `TypeFamilies`.

`beam-postgres` is built on top of `postgresql-simple`, which is used for connection management, transaction support, serialization, and deserialization.

> 💡 We assume that you’ve seen the part on `postgresql-simple`.
> 
### How to connect to a database

We use `postgresql-simple` straight away. Reminder:

```haskell
connectionInfo :: ConnectInfo
connectionInfo =
  defaultConnectInfo
    { connectHost = Hardcoded.host
    , connectDatabase = Hardcoded.database
    , connectUser = Hardcoded.user
    , connectPassword = Hardcoded.password
    }
```

```haskell
Simple.withConnect connectionInfo $ \connection -> do
    doFoo connection
    doBar connection
```

### How to define tables

Let’s look at the definition of the **product** table:

```haskell
data ProductT f = Product
  { id :: Columnar f Int64
  , label :: Columnar f Text
  , description :: Columnar f (Maybe Text)
  }
  deriving (Generic)
  deriving anyclass (Beamable)

type Product = ProductT Identity
deriving instance Show Product

instance Table ProductT where
  data PrimaryKey ProductT f = ProductId (Columnar f Int64)
    deriving (Generic)
    deriving anyclass (Beamable)
  primaryKey = ProductId . (.id)
```

`ProductT` is a beam table. All beam tables must implement the `Beamable` typeclass (derived via generics) and the `Table` typeclass. The `Table` instance declares the type of primary keys for the table and a function that extracts them. We can use `Product` to construct values of type `Product`.

> 💡 For details, see [beam tutorial](https://haskell-beam.github.io/beam/tutorials/tutorial1/).

All the other tables look quite similar; see the [repo](https://github.com/Zelenya/elephants) for the rest of the boilerplate. One interesting bit is foreign keys / referencing other primary keys; for example, `product_id` and `category_id` in the mapping table look like are defined as `PrimaryKey ProductT f` (not `Columnar f Int64`):

```haskell
data ProductCategoryT f = ProductCategory
  { product_id :: PrimaryKey ProductT f
  , category_id :: PrimaryKey CategoryT f
  }
  deriving (Generic)
  deriving anyclass (Beamable)
```

After declaring all the tables, we describe our database:

```haskell
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
```

`WarehouseDb` needs to define all the tables and an instance of `Database _`.

> 💡 Note that you don’t need to hardcode `Postgres` and can keep the database more generic.

If you don’t have an existing database, you might get away with only `defaultDbSettings` as `DatabaseSettings`. Beam can guess a lot about the tables if we follow their conventions. But we need to override a few generated table fields in our case.

Remember that we have a couple of foreign keys? Beam adds a suffix `__id` to these, meaning if we have a record field `product_id`, generated queries will try to use the column `product_id__id`. So, we must override these in the *product_category** mapping and **warehouse** tables.

> 💡 See [beam defaults](https://haskell-beam.github.io/beam/user-guide/models/#defaults) for more information.

### How to modify data

For raw queries, we can use `postgresql-simple`:

```haskell
cleanUp :: Connection -> IO ()
cleanUp connection =
  void $ Simple.execute_ connection "truncate warehouse, product_category, product, category"
```

Let’s insert some products:

```haskell
insert1 :: Connection -> IO ()
insert1 connection =
  runBeamPostgres connection
    $ runInsert
    $ insert (warehouseDb.product)
    $ insertValues
      [ Product 1 "Wood Screw Kit 1" (Just "245-pieces")
      , Product 2 "Wood Screw Kit 2" Nothing
      ]
```

We construct the statement using `insert`, which accepts a table and values. We use `insertValues` to supply concrete values (including ids). `runInsert` runs the statement (in [`MonadBeam`](https://hackage.haskell.org/package/beam-core-0.10.1.0/docs/Database-Beam-Backend-SQL.html#t:MonadBeam)), which `runBeamPostgres` executes using the given connection.

> 💡 Note that we can use `runBeamPostgresDebug putStrLn` instead of `runBeamPostgres` to see what sql queries get executed.

`runInsert` doesn’t return anything (no affected rows, no ids, nothing). When we want some confirmation back, we can use [`runInsertReturningList`](https://hackage.haskell.org/package/beam-core-0.10.1.0/docs/Database-Beam-Backend-SQL-BeamExtensions.html#v:runInsertReturningList):

```haskell
insert2 :: Connection -> IO ()
insert2 connection = do
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
```

We can use `insertExpressions` function to insert arbitrary sql expressions. In this case, we pass `default_` to ask the database to give us default ids.

### How to query data

Instead of talking about `Q` monads and `MonadBeam`, let’s look at the examples. First, query all the products:

```haskell
query1 :: (MonadBeam Postgres m) => m [Product]
query1 = do
  let allProducts = all_ (warehouseDb.product)
  runSelectReturningList $ select allProducts
```

```haskell
runBeamPostgres connection query
```

Important bits: 

- build a query;
- pass it into `select`;
- run it in `MonadBeam` (using `runSelectReturningList`, `runSelectReturningOne`, etc);
- execute using `runBeamPostgres connection`.

For example, to build a query, we can use `all_` to introduce all entries of a table together with `guard_` to filter the results:

```haskell
query2 label = runSelectReturningList $ select $ do
  aProduct <- all_ warehouseDb.product
  guard_ (aProduct.label ==. val_ label)
  pure (aProduct.label, aProduct.description)
```

`filter_` is built on top of `guard_` and allows us to use the `in_` clause:

```haskell
query3 labels =
  runSelectReturningList
    $ select
    $ filter_ (\p -> p.label `in_` predicate)
    $ all_ warehouseDb.product
 where
  predicate = val_ <$> labels
```

Note that we use `val_` to “lift” haskell values into “sql query land”.

### How to use transactions

We use `postgresql-simple` for transactions:

```haskell
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
```

We use `currentTimestamp_` to ask the database for the current time and `pk` to get the entity's primary key. For example, we pass `pk newProduct` into the `ProductCategory` mapping. 

### How to query using joins

There are various ways to get data from multiple tables using Beam.

For example, we can use `related_` to get all entries of the given table referenced by the given primary key and `leftJoin_` to introduce a table using a left join:

```haskell
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
```

Which generates the following query:

```haskell
SELECT 
  "t0"."quantity" AS "res0", 
  "t1"."label" AS "res1", 
  "t1"."description" AS "res2", 
  "t3"."label" AS "res3" 
FROM 
  "warehouse" AS "t0" 
  INNER JOIN "product" AS "t1" ON ("t0"."product_id") = ("t1"."id") 
  LEFT JOIN "product_category" AS "t2" ON ("t2"."product_id") = ("t1"."id") 
  LEFT JOIN "category" AS "t3" ON ("t3"."id") IS NOT DISTINCT 
FROM 
  ("t2"."category_id") 
WHERE 
  ("t0"."quantity") > (3)
```

We can also use the [`manyToMany_`](https://haskell-beam.github.io/beam/user-guide/queries/relationships/#many-to-many) construct to fetch sides of a many-to-many relationship.  

```haskell
productCategoryRelationship :: ManyToMany Postgres WarehouseDb ProductT CategoryT
productCategoryRelationship =
  manyToMany_ (warehouseDb.product_category) (.product_id) (.category_id)
```

```haskell
query2 quantity = runBeamPostgres connection
  $ runSelectReturningList
  $ select
  $ do
    warehouse <- all_ warehouseDb.warehouse
    products <- related_ warehouseDb.product warehouse.product_id
    categories <- all_ warehouseDb.category
    (aProduct, category) <- productCategoryRelationship (pure products) (pure categories)
    guard_ (warehouse.quantity >. quantity)
    pure (warehouse.quantity, aProduct.label, aProduct.description, category.label)
```

Which generates the following query:

```haskell
SELECT 
  "t0"."quantity" AS "res0", 
  "t1"."label" AS "res1", 
  "t1"."description" AS "res2", 
  "t2"."label" AS "res3" 
FROM 
  "warehouse" AS "t0" 
  INNER JOIN "product" AS "t1" ON ("t0"."product_id") = ("t1"."id") 
  CROSS JOIN "category" AS "t2" 
  INNER JOIN "product_category" AS "t3" ON (
    ("t3"."product_id") = ("t1"."id")
  ) 
  AND (
    ("t3"."category_id") = ("t2"."id")
  ) 
WHERE 
  ("t0"."quantity") > (3)
```

### Errors

It’s not possible to write an invalid sql query, but this comes at a cost — compile-time errors.

For example, once we forgot to pass a parameter, and this resulted in:

```haskell
Couldn't match expected type: Q Postgres
                                    WarehouseDb
                                    QBaseScope
                                    a0
                with actual type: Q Postgres
                                    WarehouseDb
                                    s0
                                    (ProductT (QExpr Postgres s0))
                                  -> Q Postgres WarehouseDb s0 (CategoryT (QExpr Postgres s0))
                                  -> Q Postgres
                                       WarehouseDb
                                       s0
                                       (ProductT (QExpr Postgres s0),
                                       CategoryT (QExpr Postgres s0))
```

Runtime sql errors are still there, re-exported from `postgresql-simple`. Review the relevant error section if you need a reminder.

```haskell
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
```

> `Caught SQL Error: SqlError {sqlState = "23505", sqlExecStatus = FatalError, sqlErrorMsg = "duplicate key value violates unique constraint \"product_label_key\"", sqlErrorDetail = "Key (label)=(Duplicate screw) already exists.", sqlErrorHint = ""}`
> 

### Resources

Beam has you covered — it comes with an overview, quick-start guide, tutorial, user guide, and hackage docs. 

*Spoiler alert: Beam is likely the best-documented library reviewed in this tutorial.*

### Migrations

The `beam-migrate` package provides a migrations framework. 

> *“The `beam-migrate` tool can generate a beam schema from a pre-existing database, manage migrations for several production databases, automatically generate migrations between two schemas, and much more.”*
> 

### In summary

`beam` states that if the query compiles, it will generate proper code. Beam uses the GHC Haskell type system and nothing else — no Template Haskell. You don’t have to write raw sql or sql like code. After defining some boilerplate, you write and compose queries in a straightforward Haskell style and get valid SQL.

Regarding complexity, let the types do the talking:

```haskell
 manyToMany_
  :: ( Database be db, Table joinThrough
     , Table left, Table right
     , Sql92SelectSanityCheck syntax
     , IsSql92SelectSyntax syntax
     , SqlEq (QExpr (Sql92SelectExpressionSyntax syntax) s) (PrimaryKey left (QExpr (Sql92SelectExpressionSyntax syntax) s))
     , SqlEq (QExpr (Sql92SelectExpressionSyntax syntax) s) (PrimaryKey right (QExpr (Sql92SelectExpressionSyntax syntax) s)) )
  => DatabaseEntity be db (TableEntity joinThrough)
  -> (joinThrough (QExpr (Sql92SelectExpressionSyntax syntax) s) -> PrimaryKey left (QExpr (Sql92SelectExpressionSyntax syntax) s))
  -> (joinThrough (QExpr (Sql92SelectExpressionSyntax syntax) s) -> PrimaryKey right (QExpr (Sql92SelectExpressionSyntax syntax) s))
  -> Q syntax db s (left (QExpr (Sql92SelectExpressionSyntax syntax) s)) -> Q syntax db s (right (QExpr (Sql92SelectExpressionSyntax syntax) s))
  -> Q syntax db s (left (QExpr (Sql92SelectExpressionSyntax syntax) s), right (QExpr (Sql92SelectExpressionSyntax syntax) s))
```

## `squeal`

*Okay, what if we did something quite similar but quite different?*

[Squeal](https://hackage.haskell.org/package/squeal-postgresql) *“is a type-safe embedding of PostgreSQL in Haskell”*, which means *“that Squeal embeds both SQL terms and SQL types into Haskell at the term and type levels respectively. This leads to a very high level of type-safety”.*

Install [`squeal-postgresql`](https://hackage.haskell.org/package/squeal-postgresql) (`0.9.1.3` released in 2023) and [`generics-sop`](https://hackage.haskell.org/package/generics-sop), which the library uses for generic encodings of Haskell tuples and records.

Enable: `DataKinds`, `GADTs`, and `OverloadedLabels`

> 💡 (It’s not very important, but) We assume you’ve seen the part on postgresql-simple, which covers the same topics but at a slower pace.

### How to connect to a database

We pass `libpq` connection string (e.g., `"host=localhost port=5432 user=postgres dbname=warehouse password=password"`) to `withConnection`:

```haskell
withConnection Hardcoded.connectionString $ 
  doFoo 
    & pqThen doBar
    & pqThen doBaz
```

We can also create a connection pool using [`createConnectionPool`](https://hackage.haskell.org/package/squeal-postgresql-0.9.1.3/docs/Squeal-PostgreSQL-Session-Pool.html#v:createConnectionPool) and use the pool with [`usingConnectionPool`](https://hackage.haskell.org/package/squeal-postgresql-0.9.1.3/docs/Squeal-PostgreSQL-Session-Pool.html#v:usingConnectionPool).

### How to define tables

First, we define table columns and constraints:

```haskell
type ProductColumns =
  '[ "id" ::: 'Def :=> 'NotNull 'PGint4
   , "label" ::: 'NoDef :=> 'NotNull 'PGtext
   , "description" ::: 'NoDef :=> 'Null 'PGtext
   ]

type ProductConstraints = '["pk_product" ::: 'PrimaryKey '["id"]]
```

`'Def` means that `DEFAULT` is available for inserts and updates, `'NoDef` — unavailable. We specify nullability with `'NotNull` and `'Null` and the primary key with `'PrimaryKey`.

We use the `:::` type operators to pair a `Symbol` with schema types, constraints, column types, etc. We use `:=>` to specify constraints as well as optionality.

All the other tables look pretty similar (with additional `'ForeignKey` constraints here and there); see the [repo](https://github.com/Zelenya/elephants) for the rest of the boilerplate.

Then, we define a schema:

```haskell
type Schema =
  '[ "product" ::: 'Table (ProductConstraints :=> ProductColumns)
   , "category" ::: 'Table (CategoryConstraints :=> CategoryColumns)
   , "product_category" ::: 'Table (ProductCategoryConstraints :=> ProductCategoryColumns)
   , "warehouse" ::: 'Table (WarehouseConstraints :=> WarehouseColumns)
   ]

type DB = Public Schema
```

We use generics to convert between Haskell and PostgreSQL values:

```haskell
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
```

```haskell
data BasicProduct = BasicProduct {label :: Text, description :: Maybe Text}
  deriving stock (Show, GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
```

The `SOP.Generic` and `SOP.HasDatatypeInfo` instances allow us to encode and decode `BasicProduct`s.

### How to modify data

We can execute raw statements:

```haskell
cleanUp :: PQ DB DB IO ()
cleanUp =
  execute_ teardown
 where
  teardown :: Statement db () ()
  teardown = manipulation $ UnsafeManipulation "truncate warehouse, product_category, product, category"
```

`Manipulation` represents update, insert, and delete statements. 

We can specify the schema changes by using concrete `PQ`; when the schema doesn't change, we can use `MonadPQ` constraint (e.g., `cleanUp :: (MonadPQ DB m) => m ())`. In the end, we’ll turn either into `IO`:

```haskell
withConnection Hardcoded.connectionString
  $ cleanUp
```

Let’s insert a product: 

```haskell
insertProduct :: Statement DB BasicProduct ()
insertProduct =
  manipulation
    $ insertInto_
      #product
      (Values_ (Default `as` #id :* Set (param @1) `as` #label :* Set (param @2) `as` #description))
```

A [`Statement`](https://hackage.haskell.org/package/squeal-postgresql-0.9.1.3/docs/Squeal-PostgreSQL-Session-Statement.html#t:Statement) is either a [`Manipulation`](https://hackage.haskell.org/package/squeal-postgresql-0.9.1.3/docs/Squeal-PostgreSQL-Statement.html#v:Manipulation) or a [`Query`](https://hackage.haskell.org/package/squeal-postgresql-0.9.1.3/docs/Squeal-PostgreSQL-Session-Statement.html#v:Query) that can be run in a [`MonadPQ`](https://hackage.haskell.org/package/squeal-postgresql-0.9.1.3/docs/Squeal-PostgreSQL-Session-Monad.html#v:MonadPQ).

We use `manipulation` and `insertInto_` to construct an insert. We pass a table and what to insert. `Values_` describes a single [n-ary product](https://hackage.haskell.org/package/squeal-postgresql-0.9.1.3/docs/Squeal-PostgreSQL-Type-List.html#t:NP), where we must match all the columns. We can use `Default` value for id and set the rest using relevant parameters.

And then, we use [`executePrepared_`](https://hackage.haskell.org/package/squeal-postgresql-0.9.1.3/docs/Squeal-PostgreSQL-Session-Monad.html#v:executePrepared_) to run a statement that returns nothing. The function prepares the statement and runs it on each element.

```haskell
insertStuff :: (MonadPQ DB m) => m ()
insertStuff = do
  executePrepared_
    insertProduct
    [ BasicProduct "Wood Screw Kit 1" (Just "245-pieces")
    , BasicProduct "Wood Screw Kit 2" Nothing
    ]
```

`insertInto_` is a specialized version of `insertInto` with [`OnConflictDoRaise`](https://hackage.haskell.org/package/squeal-postgresql-0.9.1.3/docs/Squeal-PostgreSQL-Manipulation-Insert.html#v:OnConflictDoRaise) (what to do in case of conflict) and no [`ReturningClause`](https://hackage.haskell.org/package/squeal-postgresql-0.9.1.3/docs/Squeal-PostgreSQL-Manipulation.html#t:ReturningClause) (what to return). `ReturningClause` returns a value based on each row; for example, we can use it to return the created `id`:

```haskell
insertCategory :: Statement DB Category (Only Int32)
insertCategory =
  manipulation
    $ insertInto
      #category
      (Values_ (Default `as` #id :* Set (param @1) `as` #label))
      OnConflictDoRaise
      (Returning_ (#id `as` #fromOnly))
```

Note that we have to use `Only` and `#fromOnly`, because we can’t use primitive types (because they don’t have named labels that the library relies on).

This time we have to use `executePrepared`, which returns a list of results:

```haskell
insertStuff :: (MonadPQ DB m, MonadIO m) => m ()
insertStuff = do
  result :: [Result (Only Int32)] <-
    executePrepared insertCategory [Category "Screws", Category "Wood Screws", Category "Concrete Screws"]
  rows <- traverse getRows result
  liftIO $ putStrLn $ "Inserted categories: " <> show rows
```

We use `getRows` to get all rows from a `Result`.

### How to query data

To retrieve data, we also write `Statement`s, this time using `query` and `select_`:

```haskell
query1 :: Statement DB () BasicProduct
query1 =
  query
    $ select_
      (#product ! #label :* #product ! #description)
      (from (table #product))
```

The query returns all the products from the table.

> 💡 Note that we can use `printSQL` to print statements and see what sql queries get executed.

---

💡 We **can’t** return tuples or primitive types because they don't have named fields. You must define a new datatype and derive Squeal typeclasses to return something new.

If you try using tuples, you get an error:

```
The type `(Text, Text)' is not a record type.
It has no labelled fields.
```

---

And then we `execute` the query:

```haskell
insertStuff :: (MonadPQ DB m, MonadIO m) => m ()
insertStuff = do
  result1 <- execute query1
  rows1 <- getRows result1
  liftIO $ putStrLn $ "Query 1: " <> show rows1
```

We can select specific fields and narrow down the results:

```haskell
query2 :: Statement DB (Only Text) BasicProduct
query2 =
  query
    $ select_
      (#product ! #label :* #product ! #description)
      (from (table #product) & where_ (#product ! #label .== (param @1)))
```

We use `where_` to filter the rows and `.==` to compare for equality. 

This time, we use `executeParams` to pass the parameters into the statement:

```haskell
queryData :: PQ DB DB IO ()
queryData = do
  result2 <- executeParams query2 (Only "Wood Screw Kit 1") >>= getRows
  liftIO $ putStrLn $ "Query 2: " <> show result2
```

We can also use `in_`:

```haskell
query3 labels =
  query
    $ select_
      (#product ! #label :* #product ! #description)
      (from (table #product) & where_ (#product ! #label `in_` labels))
```

```haskell
do
  (result3 :: [BasicProduct]) <- execute (query3 ["Wood Screw Kit 2", "Wood Screw Kit 3"]) >>= getRows
  liftIO $ putStrLn $ "Query 3: " <> show result3
```

### How to use transactions

We can wrap computation in `transactionally_`:

```haskell
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
```

In case of exception, it rollbacks the transaction and rethrows the exception.

### How to query using joins

We use `innerJoin` and `leftOuterJoin` to join the tables:

```haskell
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
```

Which generates:

```sql
SELECT "w"."quantity"    AS "quantity",
       "p"."label"       AS "label",
       "p"."description" AS "description",
       "c"."label"       AS "category"
FROM   "warehouse" AS "w"
       inner join "product" AS "p"
               ON ( "w"."product_id" = "p"."id" )
       left outer join "product_category" AS "pc"
                    ON ( "pc"."product_id" = "p"."id" )
       left outer join "category" AS "c"
                    ON ( "c"."id" = "pc"."category_id" )
WHERE  ( "w"."quantity" > ( $1 :: int4 ) )
```

### Errors

If you forget or mistype anything, most of the time, the error messages are rarely simple.

Sometimes, they overwhelm:

```haskell
_ :: NP
(Aliased
(Optional
(Expression
'Ungrouped
'[]
'[]
'["public"
::: '["product" ::: 'Table (ProductConstraints :=> ProductColumns),
"category" ::: 'Table (CategoryConstraints :=> CategoryColumns),
"product_category"
::: 'Table (ProductCategoryConstraints :=> ProductCategoryColumns),
"warehouse"
::: 'Table (WarehouseConstraints :=> WarehouseColumns)]]
'[ 'NotNull 'PGtext, 'Null 'PGtext]
from0)))
'["description" ::: ('NoDef :=> 'Null 'PGtext)]
Where: ‘from0’ is an ambiguous type variable
```

Sometimes, they leak:

```haskell
Couldn't match type: TupleOf (TupleCodeOf Text (SOP.Code Text))
               with: null10 'PGtext : xs0
```

```haskell
Ambiguous type variable ‘y0’ arising from a use of ‘manipulation’
prevents the constraint ‘(SOP.Generic y0)’ from being solved.
```

```haskell
Couldn't match type: '["description"
::: ('NoDef :=> 'Null 'PGtext)]
with: '[]
```

Sometimes, they **really** leak:

```haskell
Couldn't match type: records-sop-0.1.1.1:Generics.SOP.Record.ExtractTypesFromRecordCode
                       (records-sop-0.1.1.1:Generics.SOP.Record.ToRecordCode_Datatype
                          y (SOP.DatatypeInfoOf y) (SOP.Code y))
                with: records-sop-0.1.1.1:Generics.SOP.Record.GetSingleton
                       (SOP.Code y)
  arising from a use of ‘manipulation’
```

But when it comes to runtime SQL errors, the library provides a convenient `SquealException` for exceptions that Squeal can throw and a nice API for working with them built on top of `exceptions`. For example, we can use `catchSqueal`:

```haskell
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
```

### Resources

The library comes with a quickstart and [Core Concepts Handbook](https://github.com/morphismtech/squeal/blob/dev/squeal-core-concepts-handbook.md).

### Migrations

The library has a [`Migration`](https://hackage.haskell.org/package/squeal-postgresql-0.9.1.3/docs/Squeal-PostgreSQL-Session-Migration.html#t:Migration) module to change the database schema over time. They support linear, pure or impure, one-way or rewindable migrations.

### In summary

Squeal is another type-safe postgres library not suitable for beginners. You should be comfortable working on the type level, reading generic-related errors, etc. The library uses generic encodings ([`generics-sop`](https://hackage.haskell.org/package/generics-sop)) of records/tuples, which keep getting into the error messages.

## `opaleye`

*Okay, what if we did something quite similar but quite different?*

Opaleye is *“an SQL-generating DSL targeting PostgreSQL. Allows Postgres queries to be written within Haskell in a typesafe and composable fashion”*.

Install [`opaleye`](https://hackage.haskell.org/package/opaleye) (`0.10.1.0` released in 2023) and `product-profunctors`, which the library uses under the hood.

`opaleye` is built on top of `postgresql-simple`, which is used for connection management, transaction support, serialization, and deserialization.

> 💡 We assume you’ve seen the part on `postgresql-simple`.

### How to connect to a database

We use `postgresql-simple` straight away. Reminder:

```haskell
connectionInfo :: ConnectInfo
connectionInfo =
  defaultConnectInfo
    { connectHost = Hardcoded.host
    , connectDatabase = Hardcoded.database
    , connectUser = Hardcoded.user
    , connectPassword = Hardcoded.password
    }
```

```haskell
Simple.withConnect connectionInfo $ \connection -> do
    doFoo connection
    doBar connection
```

### How to define tables

We define a table using the `table` function — specify the table name and the type of fields. 

```haskell
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
```

`Table` takes **write fields** and **view fields**; for example, the first parameter (`id`) is auto-generated, so it’s optional on write (so we specify `Maybe (Field SqlInt4))` but always there on view/read (so we specify `Field SqlInt4`).

`p5` is a tiny glue function from `product-profunctors`; the number corresponds to the tuple arity  (number of columns). *We don’t need to know more than that.*

`tableField` infers a required or an optional field depending on the write type.

*(We’ll cover `ProductId` a bit later)*

We can also use records instead of tuples; for example, for product:

```haskell
data Product' a b c = Product {pId :: a, pLabel :: b, pDescription :: c}
type Product = Product' ProductId Text (Maybe Text)
deriving instance Show Product

type ProductFieldWrite = Product' (ProductId' (Maybe (Field SqlInt4))) (Field SqlText) (FieldNullable SqlText)
type ProductField = Product' (ProductIdField) (Field SqlText) (FieldNullable SqlText)
```

*Note that we prefix field names because we’ll have some derivable code that can’t handle duplicate record fields.*

`Product'` is polymorphic in all its fields. We’ll use `Product` in “normal” code and `ProductField` when interacting with a database. Because **id** is optional on write, we distinguish between `ProductFieldWrite` and `ProductField`.

We indicate nullable fields with `FieldNullable`, which will be converted into `Maybe` when executed.

We need some typeclass instances, which we can get with Template Haskell:

```haskell
$(makeAdaptorAndInstance "pProduct" ''Product')
```

---

💡 If you’d rather write these by hand, see [Data.Profunctor.Product.TH](https://hackage.haskell.org/package/product-profunctors/docs/Data-Profunctor-Product-TH.html).

---

And then, we define the table:

```haskell
productTable :: Table ProductFieldWrite ProductField
productTable =
  table "product"
    $ pProduct
      Product
        { pId = pProductId $ ProductId $ tableField "id"
        , pLabel = tableField "label"
        , pDescription = tableField "description"
        }
```

Note that instead of `pN`, we use `pProduct`, which we just generated with TH.

The library’s basic tutorial suggests using newtypes for ids. For example, we use one for the product id:

```haskell
newtype ProductId' a = ProductId a
$(makeAdaptorAndInstance "pProductId" ''ProductId')
type ProductId = ProductId' Int
deriving instance Show ProductId

type ProductIdField = ProductId' (Field SqlInt4)
```

*See the [repo](https://github.com/Zelenya/elephants) for the rest of the boilerplate.*

### How to modify data

For raw queries, we can use `postgresql-simple`:

```haskell
cleanUp :: Connection -> IO ()
cleanUp connection =
  void $ Simple.execute_ connection "truncate warehouse, product_category, product, category"
```

Otherwise, we create an insert by using `Insert`:

```haskell
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
```

We specify the tables, rows to be inserted, conflict-handling strategy, and what to return. In this case, we return product id using `rReturning`.

And then we run the insert with `runInsert`:

```haskell
insertStuff :: Connection -> IO ()
insertStuff connection = do
  result1 <- runInsert connection insert1
  putStrLn $ "Inserted products: " <> show result1
```

If we want to return the number of affected rows, we can use `rCount`:

```haskell
insert2 :: Insert Int64
insert2 =
  Insert
    { iTable = categoryTable
    , iRows = [Category (CategoryId 1) "Screws", Category (CategoryId 2) "Wood Screws", Category (CategoryId 3) "Concrete Screws"]
    , iReturning = rCount
    , iOnConflict = Nothing
    }
```

### How to query data

The basic select is simple:

```haskell
selectProduct :: Select ProductField
selectProduct = selectTable productTable
```

> 💡 Note that we can use `showSql` to print `Select` and see what sql queries get executed.

We run a select with `runSelect`:

```haskell
queryData :: Connection -> IO ()
queryData connection = do
  result1 :: [Product] <- runSelect connection selectProduct
  putStrLn $ "Query 1: " <> show result1
```

`runSelect` “*converts a "record" of Opaleye fields to a list of "records" of Haskell values.”* We must specify the return type (`[Product]`) to help type inference. 

We can select specific fields and narrow down the results:

```haskell
select2 :: Select (Field SqlText, FieldNullable SqlText)
select2 = do
  (Product _ aLabel description) <- selectProduct
  where_ (aLabel .== "Wood Screw Kit 2")
  pure (aLabel, description)
```

```haskell
result2 :: [(Text, Maybe Text)] <- runSelect connection select2
```

We use `where_` to filter the rows and `.==` to compare for equality. We can also use `in_`:

```haskell
select3 :: Select (Field SqlText)
select3 = do
  p <- selectProduct
  where_ $ in_ ["Wood Screw Kit 2", "Wood Screw Kit 3"] p.pLabel
  pure (p.pLabel)
```

```haskell
result3 :: [Text] <- runSelect connection select3
```

### How to use transactions

We use `postgresql-simple` for transactions:

```haskell
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
```

### How to query using joins

Opaleye provides a couple of APIs for joins. They recommend using [`where_`](https://hackage.haskell.org/package/opaleye-0.10.0.0/docs/Opaleye-Operators.html#v:where_) directly for inner joins and [`optional`](https://hackage.haskell.org/package/opaleye-0.10.0.0/docs/Opaleye-Join.html#v:optional) for left/right joins. Which gives us something like this:

```haskell
join :: Select (Field SqlInt4, Field SqlText, FieldNullable SqlText, FieldNullable SqlText)
join = do
  (_, wProductId, quantity, _, _) <- selectTable warehouseTable
  p <- selectTable productTable
  c <- optional $ selectTable categoryTable
  pc <- optional $ selectTable productCategoryTable

  where_ $ wProductId .=== p.pId
  where_ $ (productId <$> pc) .=== (pure p.pId)
  where_ $ (categoryId <$> pc) .=== (cId <$> c)
  where_ $ quantity .> 3

  let category = maybeFieldsToNullable $ cLabel <$> c
  pure (quantity, p.pLabel, p.pDescription, category)
```

Which generates:

```haskell
SELECT
"quantity2_1" as "result1_7",
"label1_2" as "result2_7",
"description2_2" as "result3_7",
CASE WHEN NOT (("rebind0_4") IS NULL) THEN "label1_3" ELSE NULL END as "result4_7"
FROM (SELECT
      *
      FROM (SELECT *
            FROM
            (SELECT *
             FROM
             (SELECT
              *
              FROM (SELECT
                    "id" as "id0_1",
                    "product_id" as "product_id1_1",
                    "quantity" as "quantity2_1",
                    "created" as "created3_1",
                    "modified" as "modified4_1"
                    FROM "warehouse" as "T1") as "T1",
                   LATERAL
                   (SELECT
                    "id" as "id0_2",
                    "label" as "label1_2",
                    "description" as "description2_2"
                    FROM "product" as "T1") as "T2") as "T1"
             LEFT OUTER JOIN
             LATERAL
             (SELECT
              TRUE as "rebind0_4",
              *
              FROM (SELECT
                    "id" as "id0_3",
                    "label" as "label1_3"
                    FROM "category" as "T1") as "T1") as "T2"
             ON
             TRUE) as "T1"
            LEFT OUTER JOIN
            LATERAL
            (SELECT
             TRUE as "rebind0_6",
             *
             FROM (SELECT
                   "product_id" as "product_id0_5",
                   "category_id" as "category_id1_5"
                   FROM "product_category" as "T1") as "T1") as "T2"
            ON
            TRUE) as "T1"
      WHERE (("quantity2_1") > (CAST(3 AS integer))) AND ((NOT (("rebind0_6") IS NULL)) = (NOT (("rebind0_4") IS NULL)) AND ((NOT (NOT (("rebind0_6") IS NULL))) OR (("category_id1_5") = ("id0_3")))) AND ((NOT (("rebind0_6") IS NULL)) = (CAST(TRUE AS boolean)) AND ((NOT (NOT (("rebind0_6") IS NULL))) OR (("product_id0_5") = ("id0_2")))) AND (("product_id1_1") = ("id0_2"))) as "T1"
```

> 🤷 TBH, still not 100% sure it’s a proper way to do it (because there are no examples), but it compiles and returns what is expected, so…
> 

### Errors

Once again, type-safety and query validation equal compilation errors. 

But because there isn’t much “type-level magic”, we only need to occasionally help the compiler with type inference. And it’s mainly about input and return types — not intermediate/internal library structures. For example, `[Product]` in this snippet:

```haskell
result :: [Product] <- runSelect connection selectProduct
 where
  selectProduct = selectTable productTable
```

Sometimes, if you don’t specify enough types, profunctors show up:

```haskell
Ambiguous type variable ‘haskells0’ arising from a use of ‘runSelect’
prevents the constraint ‘(Default
                            FromFields fields0 Product)’ from being solved.
Probable fix: use a type annotation to specify what ‘haskells0’ should be.
```

Runtime sql errors are again from `postgresql-simple`. Review the relevant error section if you need a reminder.

```haskell
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
```

### Resources

There are a couple of basic tutorials in the repo and some external ones.

One thing to remember: sometimes, the library provides multiple ways of doing things (for example, left joins using `optional` vs. deprecated `leftJoin` or monadic vs. arrow syntax), and documentation/tutorials can do it one way or even deprecated way.

### Migrations

Opaleye assumes a database already exists — no support for migrations or creating tables and databases.

### In summary

Opaleye allows us to define tables and write type-safe postgres queries using Haskell code.

The library uses `product-profunctors` and typeclasses. Both only come up in copy-pasteable boilerplate and when you under-specify the return types. No deep knowledge is required.

## `rel8`

*Okay, what if we did something quite similar but quite different?*

Rel8 “*is a Haskell library for interacting with PostgreSQL databases”*, which aims to be concise,  inferrable, and familiar.

For the database connection, instead of `postgresql-simple`, `rel8` uses Hasql.

Install `rel8` (`1.4.1.0` released in 2023), `hasql`, and `hasql-transaction`. 

We bring back the `TypeFamilies` extension and (in case you haven’t already) `DuplicateRecordFields`. The latter is required to disambiguate the record fields when working with inserts, updates, and deletes…

> 💡 We assume you’ve seen the parts on `postgresql-simple`, `hasql`, and `opaleye`.

### How to connect to a database

We use Hasql. Reminder:

```haskell
Right connection <- getConnection
```

```haskell
getConnection :: IO (Either ConnectionError Connection)
getConnection =
  acquire $ settings Hardcoded.host Hardcoded.portNumber Hardcoded.user Hardcoded.password Hardcoded.database
```

### How to define tables

First, we describe the structural mapping of the tables. Take for instance `Product`:

```haskell
newtype ProductId = ProductId Int64
  deriving newtype (DBEq, DBType, Eq, Show)

data Product f = Product
  { id :: Column f ProductId
  , label :: Column f Text
  , description :: Column f (Maybe Text)
  }
  deriving (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (Product f)
```

We define fields with  `Column` and derive the `Rel8able` instance. We also declare a newtype for product id with a few instances. 

Imagine that the last line is just `deriving (Show)`.

Then, we describe a `TableSchema` for each table. The relevant table looks like this:

```haskell
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
```

Note that defining columns looks repetitive — we can use some generics machinery to get that information from the `Rel8able`:

```haskell
productSchema :: TableSchema (Product Name)
productSchema =
  TableSchema
    { name = "product"
    , schema = Nothing
    , columns = namesFromLabels @(Product Name)
    }
```

> 💡 `namesFromLabels` generates a table schema where every column name corresponds precisely to the field's name. Alternatively, we can use `namesFromLabelsWith`.

See the [repo](https://github.com/Zelenya/elephants) for the rest of the boilerplate.
### How to modify data

For raw queries, we can use Hasql:

```haskell
cleanUp :: Connection -> IO (Either QueryError ())
cleanUp connection = run cleanUpSession connection
 where
  cleanUpSession = statement () $ Statement rawSql E.noParams D.noResult True
  rawSql = "truncate warehouse, product_category, product, category"
```

Otherwise, we create `Insert`:

```haskell
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
```

We’ve seen this in Opaleye’s insert: the table, rows to insert, conflict-handling strategy, and what to return. 

We use `unsafeDefault` for sql `DEFAULT`, [`lit`](https://hackage.haskell.org/package/rel8-1.4.1.0/docs/Rel8.html#v:lit) to turn Haskell values into expressions, and [`values`](https://hackage.haskell.org/package/rel8-1.4.1.0/docs/Rel8.html#v:values) to construct a query out of the given rows.

---

💡 Note that [`unsafeDefault`](https://hackage.haskell.org/package/rel8-1.4.1.0/docs/Rel8.html#v:unsafeDefault) is named unsafe for a reason; see the docs.

---

And run this like any other Hasql statement:

```haskell
result1 <- run (statement () insert1) connection
```

If we want to return the number of affected rows, we can use `NumberOfRowsAffected`:

```haskell
Insert
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
```

### How to query data

We build select statements using `Query`. We select all rows from a table using `each` and turn (run) the query into `Statement` using `select`:

```haskell
select1 :: Statement () [Product Result]
select1 = select $ each productSchema
```

> 💡 Note that we can use `showQuery` to print sql queries that will be executed.

And once again we run the statement:

```haskell
result1 <- run (statement () select1) connection
```

We can select specific fields and narrow down the results:

```haskell
select2 :: Statement () [(Text, Maybe Text)]
select2 = select $ do
  p <- each productSchema
  where_ $ p.label ==. "Wood Screw Kit 2"
  pure (p.label, p.description)
```

We use `where_` to filter the rows and `==.` to compare for equality. We can also use `in_`:

```haskell
select3 :: Statement () [Text]
select3 = select $ do
  p <- each productSchema
  where_ $ p.label `in_` ["Wood Screw Kit 2", "Wood Screw Kit 3"]
  pure p.label
```

*Note that the order of parameters is different from Opaleye.*

### How to use transactions

We use Hasql for transactions:

```haskell
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

    -- insert category
    -- insert mapping
    -- insert warehouse listing
```

### How to query using joins

Rel8 doesn’t have a specific join operation — we use `where_` (or `filter`) to filter the results and `optional` to do what outer joins do.

```haskell
queryWithJoins :: Connection -> IO ()
queryWithJoins connection = do
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
```

We extract “each join” into a specialized function to make the code cleaner (according to the Rel8 tutorials). We use `optional` and `traverseMaybeTable` to account for the partiality of queries. `MaybeTable` results from an outer join, which we unwrap with `maybeTable`.

`filter` is an alternative way to write `where` clauses.

The generated query:

```haskell
SELECT
CAST("quantity2_1" AS int4) as "_1",
CAST("label1_3" AS text) as "_2",
CAST("description2_3" AS text) as "_3",
CAST(CASE WHEN ("rebind0_8") IS NULL THEN CAST(NULL AS text) ELSE "label1_12" END AS text) as "_4"
FROM (SELECT
      *
      FROM (SELECT *
            FROM
            (SELECT *
             FROM
             (SELECT
              *
              FROM (SELECT
                    "id" as "id0_1",
                    "product_id" as "product_id1_1",
                    "quantity" as "quantity2_1",
                    "created" as "created3_1",
                    "modified" as "modified4_1"
                    FROM "warehouse" as "T1") as "T1",
                   LATERAL
                   (SELECT
                    "id" as "id0_3",
                    "label" as "label1_3",
                    "description" as "description2_3"
                    FROM "product" as "T1") as "T2"
              WHERE (("id0_3") = ("product_id1_1"))) as "T1"
             LEFT OUTER JOIN
             LATERAL
             (SELECT
              TRUE as "rebind0_8",
              *
              FROM (SELECT
                    *
                    FROM (SELECT
                          "product_id" as "product_id0_6",
                          "category_id" as "category_id1_6"
                          FROM "product_category" as "T1") as "T1"
                    WHERE (("product_id0_6") = ("id0_3"))) as "T1") as "T2"
             ON
             TRUE) as "T1"
            LEFT OUTER JOIN
            LATERAL
            (SELECT
             TRUE as "rebind0_14",
             *
             FROM (SELECT
                   *
                   FROM (SELECT
                         0) as "T1",
                        LATERAL
                        (SELECT
                         "id" as "id0_12",
                         "label" as "label1_12"
                         FROM "category" as "T1") as "T2"
                   WHERE (("id0_12") = ("category_id1_6")) AND (("rebind0_8") IS NOT NULL)) as "T1") as "T2"
            ON
            TRUE) as "T1"
      WHERE (("quantity2_1") > (CAST(3 AS int4))) AND (((("rebind0_14") IS NULL) AND (("rebind0_8") IS NULL)) OR ((("rebind0_14") = ("rebind0_8")) AND (COALESCE(("rebind0_14") = ("rebind0_8"),FALSE))))) as "T1"
```

Which looks similar to the relevant Opaleye query in the previous section.

### Errors

On top of type-safety, according to the docs, *“Rel8 aims to have excellent and predictable type inference”*. And they deliver — type inference rarely needs any guidance, and the compilation errors are pretty good. 

Although it’s possible to introduce runtime errors using unsafe operations like unsafeDefault, the name is explicit, well documented, and has proper alternatives.

Runtime errors come from Hasql — all error-reporting is explicit and is presented using `Either`. As a reminder, violating the constraint returns a familiar error:

```haskell
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
```

> `Constraint violation (Left): QueryError "INSERT INTO \"product\" (\"id\",\n \"label\",\n \"description\")\nVALUES\n(DEFAULT,CAST(E'Duplicate screw' AS text),CAST(NULL AS text))" [] (ResultError (ServerError "23505" "duplicate key value violates unique constraint \"product_label_key\"" (Just "Key (label)=(Duplicate screw) already exists.") Nothing Nothing))`
> 

### Resources

Rel8 has the Getting Started tutorial, the Concepts documentation, the cookbook, and good API docs. This would have been one of the best coverages, but unfortunately, some basic snippets (like running selects or constructing inserts) aren’t valid anymore.

Also, you have to keep in mind `hasql`.

### Migrations

Rel8 assumes a database already exists — no support for migrations or creating tables and databases.

### In summary

Rel8 also allows us to write type-safe postgres queries using *concise*, *inferrable*, and *familiar* Haskell code. It builds on top of `opaleye` and `hasql`, and you must be somewhat familiar with the latter.

## `selda`

*Okay, what if we did something quite similar but quite different?*

[Selda](https://selda.link/) *“is a Haskell library for interacting with SQL-based relational databases”* (PostgreSQL or SQLite). *“The library was inspired by [LINQ](https://en.wikipedia.org/wiki/Language_Integrated_Query) and [Opaleye](http://hackage.haskell.org/package/opaleye).”*

Install `selda` (`0.5.2.0` released in 2022) and `selda-postgresql`.

Enable `OverloadedLabels`.

### How to connect to a database

Create connection info:

```haskell
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
```

And use it with `withPostgreSQL`:

```haskell
withPostgreSQL connectionInfo $ do
  doFoo
  doBar
```

### How to define tables

First, we declare normal types and derive `SqlRow`, for example, for product:

```haskell
data Product = Product
  { id :: ID Product
  , label :: Text
  , description :: Maybe Text
  }
  deriving (Generic, Show)
  deriving anyclass (SqlRow)
```

Then, we use table to declare a table:

```haskell
productTable :: Table Product
productTable = table "product" [#id :- autoPrimary]
```

We specify constraints by linking selectors of the table to the definitions. We use `autoPrimary` for auto-incrementing primary keys, `primary` for regular primary keys, and `foreignKey` for foreign keys:

```haskell
mappingTable :: Table ProductCategory
mappingTable =
  table
    "product_category"
    [ #product_id :- foreignKey productTable #id
    , #category_id :- foreignKey categoryTable #id
    ]
```

*See the [repo](https://github.com/Zelenya/elephants) for the rest of the boilerplate.*

### How to modify data

We can use `rawStm` from `Database.Selda.Unsafe` to execute raw queries:

```
cleanUp :: SeldaM PG ()
cleanUp =
  rawStm "truncate warehouse, product_category, product, category"
```

[`SeldaM`](https://hackage.haskell.org/package/selda-0.5.2.0/docs/Database-Selda.html#t:SeldaM) is an alias for `SeldaT IO`, `SeldaT` is a Selda computation — a concrete implementation (of `MonadSelda`) with Selda SQL capabilities.

At the end we’ll turn it into `IO`:

```haskell
withPostgreSQL connectionInfo $ do
  cleanUp
```

To insert data, we can use `insert_` that doesn’t return anything, `insert` that returns the number of inserted rows, and `insertWithPK` that returns the primary key of the last inserted row.

```haskell
insertStuff :: SeldaM PG ()
insertStuff = do
  productId <-
    insertWithPK
      productTable
      [ Product def "Wood Screw Kit 1" (Just "245-pieces")
      , Product def "Wood Screw Kit 2" Nothing
      ]
  liftIO $ putStrLn $ "Inserted product with id: " <> show productId
```

```haskell
  rows <-
    insert
      categoryTable
      [Category def "Screws", Category def "Wood Screws", Category def "Concrete Screws"]
  liftIO $ putStrLn $ "Inserted categories: " <> show rows
```

We use `def` when we want to use the default value, which is the case with ids.

### How to query data

We can get all the rows from the given table using `select`:

```haskell
  selectProduct :: Query t (Row t Product)
  selectProduct = select productTable
```

> 💡 Note that we can use `compile` from `Database.Selda.Debug` to print sql queries that will be executed.

And execute the query with `query`:

```haskell
queryData :: SeldaT PG IO ()
queryData = do
  result1 <- query selectProduct
  liftIO $ putStrLn $ "Query 1: " <> show result1
```

---

🤷 Somehow, here, Selda didn’t want to read/parse back the ids it just inserted:

> `elephants-exe: [SELDA BUG] fromSql: RowID column with non-int value: SqlInt32...`
> 

If we change the type from `ID Foo` to `Int32`, the select works, but then insert with auto-incremental primary keys and other functionality doesn’t 🤷 

*So let’s ignore this for now because other queries work fine.*

---

We can select specific fields and narrow down the results:

```haskell
select2 :: Query t (Col t Text :*: Col t (Maybe Text))
select2 = do
  p <- selectProduct
  restrict (p ! #label .== "Wood Screw Kit 2")
  pure (p ! #label :*: p ! #description)
```

`Query` is parameterized over a scope parameter `t`, ensuring that queries are always well-scoped, but we don’t have to worry about it now.

We use `!` with selectors to extract a column, `restrict` to filter the rows, and `.==` to compare for equality. A result is an inductive tuple — one or more values separated by the `:*:` data constructor.

We can also use `isIn`:

```haskell
select3 = do
  p <- selectProduct
  restrict (p ! #label `isIn` ["Wood Screw Kit 2", "Wood Screw Kit 3"])
  pure (p ! #label)
```

### How to use transactions

We use `transaction`:

```haskell
insertWithTransaction :: SeldaT PG IO ()
insertWithTransaction = transaction $ do
  productId <- insertWithPK productTable [Product def "Drywall Screws Set" (Just "8000pcs")]
  categoryId <- insertWithPK categoryTable [Category def "Drywall Screws"]
  insert_ mappingTable [ProductCategory productId categoryId]
  insert_ warehouseTable [Warehouse def productId 10 def def]
  liftIO $ putStrLn $ "Insert with transaction"
```

### How to query using joins

We use `restrict` and `leftJoin` to query with joins:

```haskell
join :: Query s (Col s Int32 :*: (Col s Text :*: (Col s (Maybe Text) :*: Col s (Coalesce (Maybe Text)))))
join = do
  w <- select warehouseTable
  p <- select productTable
  restrict (w ! #product_id .== p ! #id)

  pc <- leftJoin (\pc -> pc ! #product_id .== p ! #id) (select mappingTable)
  c <- leftJoin (\c -> just (c ! #id) .== pc ? #category_id) (select categoryTable)

  pure (w ! #quantity :*: p ! #label :*: p ! #description :*: c ? #label)
```

We use `?` to extract a column from the nullable row.

The generated query:

```haskell
SELECT 
  "quantity_2", 
  "label_6", 
  "description_7", 
  "label_13_15" 
FROM 
  (
    SELECT 
      "id_12_14", 
      "label_13_15", 
      "category_id_9_11", 
      "label_6", 
      "description_7", 
      "quantity_2" 
    FROM 
      (
        SELECT 
          "product_id_8_10", 
          "category_id_9_11", 
          "id_5", 
          "label_6", 
          "description_7", 
          "quantity_2" 
        FROM 
          (
            SELECT 
              "id_5", 
              "label_6", 
              "description_7", 
              "product_id_1", 
              "quantity_2" 
            FROM 
              (
                SELECT 
                  "product_id" AS "product_id_1", 
                  "quantity" AS "quantity_2" 
                FROM 
                  "warehouse"
              ) AS q0, 
              (
                SELECT 
                  "id" AS "id_5", 
                  "label" AS "label_6", 
                  "description" AS "description_7" 
                FROM 
                  "product"
              ) AS q1 
            WHERE 
              ("product_id_1" = "id_5")
          ) AS q3 
          LEFT JOIN (
            SELECT 
              "product_id_8" AS "product_id_8_10", 
              "category_id_9" AS "category_id_9_11" 
            FROM 
              (
                SELECT 
                  "product_id" AS "product_id_8", 
                  "category_id" AS "category_id_9" 
                FROM 
                  "product_category"
              ) AS q2
          ) AS q4 ON "product_id_8_10" = "id_5"
      ) AS q6 
      LEFT JOIN (
        SELECT 
          "id_12" AS "id_12_14", 
          "label_13" AS "label_13_15" 
        FROM 
          (
            SELECT 
              "id" AS "id_12", 
              "label" AS "label_13" 
            FROM 
              "product"
          ) AS q5
      ) AS q7 ON (
        Cast("id_12_14" AS INT)
      ) = "category_id_9_11"
  ) AS q8
```

### Errors

From Selda’s tutorial: *“While the types keep queries nice and safe, Haskell's type errors can be a bit daunting even under the best circumstances.”* In practice, type inference rarely needed guidance, and the compilation errors were relatively clear. 

The only problem we’ve encountered was the mismatch of `ID` and `SqlInt32`.

All Selda functions may throw [`SeldaError`](https://hackage.haskell.org/package/selda-0.5.2.0/docs/Database-Selda.html#t:SeldaError):

```haskell
errors :: SeldaM PG ()
errors = do
  insertDuplicateScrew
  insertDuplicateScrew
    `catch` (\(err :: SeldaError) -> liftIO $ putStrLn $ "Caught Selda Error: " <> displayException err)
 where
  insertDuplicateScrew = insert_ productTable [Product def "Duplicate screw" Nothing]
```

> `elephants-exe: SqlError "error executing query INSERT INTO \"product_category\" (\"product_id\", \"category_id\") VALUES ($1, $2)': ERROR: insert or update on table \"product_category\" violates foreign key constraint \"product_category_category_id_fkey\"\nDETAIL: Key (category_id)=(748) is not present in table \"category\".\n"`
> 

### Resources

Selda comes with a simple overview and example. There is also a [tutorial](https://selda.link/tutorial/).

### Migrations

The library has a [`Migrations`](https://hackage.haskell.org/package/selda-0.5.2.0/docs/Database-Selda-Migrations.html) module for upgrading a table from one schema to another. 

### In summary

Selda allows us to write type-safe queries in a *linear*, *natural* style. 

Depending on your experience and situation, you can use SeldaM straight, or you may need to get familiar with mtl, exceptions, lifting/unlifting IO, etc.

## Honorable mentions

At some point, I ran out of steam and didn’t have the energy to make these things work.

I didn’t manage to connect to the database using [postgresql-typed](https://hackage.haskell.org/package/postgresql-typed). And I didn’t manage to build [Haskell Relational Record](http://khibino.github.io/haskell-relational-record/) and [groundhog](https://hackage.haskell.org/package/groundhog) with `ghc 9.4.5`. Neither had releases this year. Also, there are [hdbc](https://github.com/hdbc/hdbc) and [postgresql-libpq](https://github.com/haskellari/postgresql-libpq) (you might have noticed the `libpq` connection string we used here and there).

*All of these sound fun. But we had enough for now. Maybe we’ll revisit and extend it later.*

## Okay, so which PostgreSQL library should I use with Haskell?

*I can’t tell you which library to use. I don’t know myself. But I can tell you the questions you (and your team) should consider:*

- Do you want to write raw sql queries? or query builder? or use some sort of ORM? Do you want to learn a new DSL?
- How type-safe do you want to be? How readable/optimizable the generated queries should be?
- Do you need bells and whistles, like built-in connection pools or migrations?
- How comfortable are you with Haskell and type-level? Is this something you can afford?
- Do you need to be database agnostic?

Also, this is just the tip of the iceberg; we haven’t discussed specific features, performance, compilation speed… And spoiler alert: generics, template haskell, and type-families don’t come for free. 

We aren’t really at the point where people care to compare the performance. But you don’t know unless you measure. Feel free to explore and get some community karma.

Another excellent contribution opportunity is documentation and tutorials, especially if you have a favorite library and want to convince others to consider it.

---

Wait, is it actually bad that we have so many libraries?
