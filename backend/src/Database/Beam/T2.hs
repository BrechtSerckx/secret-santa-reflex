{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Database.Beam.T2
  ( T2(..)
  ) where

import           Database.Beam.Backend.SQL.Row
import           Database.Beam.Backend.Types
import           Database.Beam.Migrate          ( BeamMigrateSqlBackend
                                                , FieldCheck
                                                )
import           Database.Beam.Migrate.Generics.Tables
                                                ( GMigratableTableSettings(..) )
import           Database.Beam.Schema.Tables    ( Beamable(..)
                                                , ChooseSubTableStrategy
                                                , Columnar'
                                                , Exposed
                                                , FieldsFulfillConstraint
                                                , GDefaultTableFieldSettings(..)
                                                , GFieldsFulfillConstraint(..)
                                                , HasConstraint
                                                , SubTableStrategyImpl
                                                , TableField
                                                , TableSkeleton
                                                , TagReducesTo
                                                , namedSubTable
                                                )
import           GHC.Generics                   ( R )

-- * MAGIC

newtype T2 k v f = T2 (k f, v f)
  deriving Generic

instance (Beamable f1, Beamable f2) => Beamable (T2 f1 f2) where
  zipBeamFieldsM
    :: Applicative m
    => (forall a . Columnar' f a -> Columnar' g a -> m (Columnar' h a))
    -> T2 f1 f2 f
    -> T2 f1 f2 g
    -> m (T2 f1 f2 h)
  zipBeamFieldsM combine (T2 (f1, f2)) (T2 (g1, g2)) =
    fmap T2
      .   (,)
      <$> zipBeamFieldsM combine f1 g1
      <*> zipBeamFieldsM combine f2 g2

  tblSkeleton :: TableSkeleton (T2 f1 f2)
  tblSkeleton = T2 (tblSkeleton @f1, tblSkeleton @f2)

instance
  ( ChooseSubTableStrategy tbl k ~ strategy1
  , SubTableStrategyImpl strategy1 f k
  , ChooseSubTableStrategy tbl v ~ strategy2
  , SubTableStrategyImpl strategy2 f v
  , TagReducesTo f (TableField tbl)
  ) => GDefaultTableFieldSettings
       (S1 f' (K1 R (k f, v f)) ())
  where
  gDefTblFieldSettings Proxy =
    M1 $ K1 (namedSubTable (Proxy @strategy1), namedSubTable (Proxy @strategy2))

instance (FieldsFulfillConstraint c k, FieldsFulfillConstraint c v) =>
  GFieldsFulfillConstraint c
           (K1 R (k Exposed, vExp Exposed)) (K1 R (k (HasConstraint c), v (HasConstraint c))) where
  gWithConstrainedFields be _ = K1
    ( to $ gWithConstrainedFields be (Proxy @(Rep (k Exposed)))
    , to $ gWithConstrainedFields be (Proxy @(Rep (v Exposed)))
    )

instance
  ( BeamMigrateSqlBackend be
  , Generic (k (Const [FieldCheck]))
  , GMigratableTableSettings be (Rep (k Identity)) (Rep (k (Const [FieldCheck])))
  , Generic (v (Const [FieldCheck]))
  , GMigratableTableSettings be (Rep (v Identity)) (Rep (v (Const [FieldCheck])))
  )
  => GMigratableTableSettings be
      (K1 R (k Identity, v Identity))
      (K1 R (k (Const [FieldCheck]), v (Const [FieldCheck])))
  where
  gDefaultTblSettingsChecks be _ embedded = K1
    ( to (gDefaultTblSettingsChecks be (Proxy @(Rep (k Identity))) embedded)
    , to (gDefaultTblSettingsChecks be (Proxy @(Rep (v Identity))) embedded)
    )

instance {-# OVERLAPS #-}
  ( BeamBackend be
  , FromBackendRow be (k Identity)
  , FromBackendRow be (v Identity)
  ) => FromBackendRow be (T2 k v Identity) where

  fromBackendRow = fmap T2 $ (,) <$> fromBackendRow <*> fromBackendRow
  valuesNeeded be Proxy = valuesNeeded be (Proxy @(k Identity))
    + valuesNeeded be (Proxy @(v Identity))
