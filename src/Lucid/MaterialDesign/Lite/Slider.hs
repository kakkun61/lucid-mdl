{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lucid.MaterialDesign.Lite.Slider
  ( slider_
  , Config (..)
  ) where
import           Data.Default.Class (Default (def))
import           GHC.Generics       (Generic)
import qualified Lucid

slider_ :: Applicative m => Config -> Lucid.HtmlT m ()
slider_ (Config attributes) =
  Lucid.input_ ([Lucid.classes_ [" mdl-slider ", " mdl-js-slider "], Lucid.type_ "range"] ++ attributes)

newtype Config =
  Config
    { attributes :: [Lucid.Attribute]
    }
  deriving (Show, Eq, Generic)

instance Default Config where
  def = Config []
