module Text.NonEmpty
  ( NonEmptyText
  , unNonEmptyText
  ) where


import qualified Data.Aeson                    as Aeson
import           Data.Refine
import qualified Data.Text                     as T

newtype NonEmptyText = NonEmptyText { unNonEmptyText :: Text}
  deriving newtype (Show, Eq)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Refined Text NonEmptyText)

instance Refine Text NonEmptyText where
  rguard text = T.null text |> "Cannot be empty."
