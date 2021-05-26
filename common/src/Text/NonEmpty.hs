module Text.NonEmpty
  ( NonEmptyText
  , unNonEmptyText
  ) where


import qualified Data.Aeson                    as Aeson
import           Data.Refine
import qualified Data.Text                     as T
import           Data.Validate

newtype NonEmptyText = NonEmptyText { unNonEmptyText :: Text}
  deriving newtype (Show, Eq)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Refined Text NonEmptyText)

instance Refine Text NonEmptyText where
  refine t =
    let t' = T.strip t
    in  if T.null t'
          then failure "Cannot be empty."
          else pure . NonEmptyText $ t'
