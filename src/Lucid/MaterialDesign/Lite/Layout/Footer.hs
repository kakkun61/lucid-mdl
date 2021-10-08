{-# LANGUAGE OverloadedStrings #-}

module Lucid.MaterialDesign.Lite.Layout.Footer
  ( logo_
  ) where
import           Data.Default.Class (Default (def))
import qualified Lucid

logo_ :: Applicative m => Config -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
logo_ (Config attributes) =
  Lucid.div_ $ Lucid.class_ " mdl-logo " : attributes

newtype Config =
  Config
    { attributes :: [Lucid.Attribute]
    }
  deriving (Show, Eq)

instance Default Config where
  def = Config []
