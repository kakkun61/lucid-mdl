{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lucid.MaterialDesign.Lite.Button
  ( button_
  , Config (..)
  , Style (..)
  , Color (..)
  ) where

import Lucid.MaterialDesign.Lite.Base (HtmlClass (toHtmlClass), Ripple)

import           Data.Default.Class (Default (def))
import           Data.Maybe         (fromMaybe)
import           GHC.Generics       (Generic)
import qualified Lucid

data Style = Raised | Fab | MiniFab | Icon deriving (Show, Read, Eq, Ord, Enum, Generic)

instance HtmlClass Style where
  toHtmlClass Raised  = " mdl-button--raised "
  toHtmlClass Fab     = " mdl-button--fab "
  toHtmlClass MiniFab = " mdl-button--mini-fab "
  toHtmlClass Icon    = " mdl-button--icon "

data Color = Colored | Primary | Accent deriving (Show, Read, Eq, Ord, Enum, Generic)

instance HtmlClass Color where
  toHtmlClass Colored = " mdl-button--colored "
  toHtmlClass Primary = " mdl-button--primary "
  toHtmlClass Accent  = " mdl-button--accent "

button_
  :: Applicative m
  => Config
  -> Lucid.HtmlT m () -- ^ Label.
  -> Lucid.HtmlT m ()
button_ (Config style color ripple attributes) label =
  let
    classes =
      [" mdl-button ", " mdl-js-button ", toHtmlClass ripple] ++
        (fromMaybe "" <$> [toHtmlClass <$> style, toHtmlClass <$> color])
  in
    Lucid.button_ (Lucid.classes_ classes : attributes) label

data Config =
  Config
    { style      :: Maybe Style
    , color      :: Maybe Color
    , ripple     :: Ripple
    , attributes :: [Lucid.Attribute]
    }
  deriving (Show, Eq, Generic)

-- |
-- >>> def :: Config
-- Config {style = Nothing, color = Nothing, ripple = Ripple False, attributes = []}
instance Default Config where
  def = Config Nothing Nothing def []
