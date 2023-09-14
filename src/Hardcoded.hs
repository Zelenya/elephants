module Hardcoded (host, port, portNumber, database, user, password, connectionString) where

import Data.String (IsString, fromString)

host :: (IsString s) => s
host = "127.0.0.1" -- "localhost"

port :: (IsString s) => s
port = "5432"

portNumber :: (Integral i) => i
portNumber = 5432

database :: (IsString s) => s
database = "warehouse"

user :: (IsString s) => s
user = "postgres"

password :: (IsString s) => s
password = "password"

-- In other words, we want a connect string like
-- "host=localhost port=5432 user=postgres dbname=warehouse password=password"
connectionString :: (IsString s) => s
connectionString = stringify [("host", host), ("port", port), ("user", user), ("dbname", database), ("password", password)]
 where
  stringify = fromString . unwords . map pair
  pair (key, value) = key <> "=" <> value
