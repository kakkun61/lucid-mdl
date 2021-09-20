{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lucid.Mdl.Button
  ( button_
  , Style (..)
  , Color (..)
  ) where

import Lucid.Mdl.Base (HtmlClass (toHtmlClass), Ripple)

import           Data.Maybe   (fromMaybe)
import           GHC.Generics (Generic)
import qualified Lucid

data Style = Raised | Fab | MiniFab | Icon deriving (Show, Read, Eq, Ord, Enum, Generic)

instance HtmlClass Style where
  toHtmlClass Raised  = "mdl-button--raised"
  toHtmlClass Fab     = "mdl-button--fab"
  toHtmlClass MiniFab = "mdl-button--mini-fab"
  toHtmlClass Icon    = "mdl-button--icon"

data Color = Colored | Primary | Accent deriving (Show, Read, Eq, Ord, Enum, Generic)

instance HtmlClass Color where
  toHtmlClass Colored = "mdl-button--colored"
  toHtmlClass Primary = "mdl-button--primary"
  toHtmlClass Accent  = "mdl-button--accent"

button_ :: Applicative m => Maybe Style -> Maybe Color -> Maybe Ripple -> [Lucid.Attribute] -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
button_ style color ripple attributes label =
  let
    classes =
      ["mdl-button", "mdl-js-button"] ++
        fromMaybe [] (sequence [toHtmlClass <$> style, toHtmlClass <$> color, toHtmlClass <$> ripple])
  in
    Lucid.button_ (Lucid.classes_ classes : attributes) label
