module SecretSanta.Backend.Database.Class
  ( IsDatabaseBackend(..)
  ) where

class IsDatabaseBackend db where
  type DBConnection db :: Type
