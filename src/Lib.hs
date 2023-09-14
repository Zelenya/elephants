module Lib (someFunc) where

import Elephants.Beam qualified as Beam
import Elephants.Hasql qualified as Hasql
import Elephants.Opaleye qualified as Opaleye
import Elephants.PersistentEsqueleto qualified as PersistentEsqueleto
import Elephants.PostgresqlSimple qualified as PostgresqlSimple
import Elephants.Rel8 qualified as Rel8
import Elephants.Selda qualified as Selda
import Elephants.Squeal qualified as Squeal

someFunc :: IO ()
someFunc = do
    PostgresqlSimple.runThis
    Hasql.runThis
    PersistentEsqueleto.runThis
    Beam.runThis
    Squeal.runThis
    Opaleye.runThis
    Rel8.runThis
    Selda.runThis
