module Text.NonEmpty
  ( NonEmptyText
  , unNonEmptyText
  ) where


import qualified Data.Aeson                    as Aeson
import           Data.Refine
import qualified Data.Text                     as T

newtype NonEmptyText = NonEmptyText { unNonEmptyText :: Text}
  deriving newtype (Show, Eq)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Refinable Text NonEmptyText)

instance Refine Text NonEmptyText where
  refine t =
    let t' = T.strip t in T.null t' |> "Cannot be empty." $> NonEmptyText t'
