{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lucid.MaterialDesign.Lite.Layout.Tab
  ( tabs_
  , tabBar_
  , tab_
  , panel_
  , TabsConfig (..)
  , TabBarConfig (..)
  , TabConfig (..)
  , PanelConfig (..)
  ) where

import Lucid.MaterialDesign.Lite.Base (Active (Active), HtmlClass (toHtmlClass), Ripple (Ripple))

import           Data.Default.Class (Default (def))
import qualified Lucid

tabs_ :: Applicative m => TabsConfig -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
tabs_ (TabsConfig attributes ripple) =
  Lucid.div_ $ Lucid.classes_ [" mdl-tabs ", " mdl-js-tabs ", toHtmlClass ripple] : attributes

tabBar_ :: Applicative m => TabBarConfig -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
tabBar_ (TabBarConfig attributes) =
  Lucid.div_ $ Lucid.class_ " mdl-tabs__tab-bar " : attributes

tab_ :: Applicative m => TabConfig -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
tab_ (TabConfig attributes active) =
  Lucid.a_ $ Lucid.classes_ [" mdl-tabs__tab ", toHtmlClass active] : attributes

panel_ :: Applicative m => PanelConfig -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
panel_ (PanelConfig attributes active) =
  Lucid.div_ $ Lucid.classes_ [" mdl-tabs__panel ", toHtmlClass active] : attributes

data TabsConfig =
  TabsConfig
    { attributes :: [Lucid.Attribute]
    , ripple     :: Ripple
    }
  deriving (Show, Eq)

instance Default TabsConfig where
  def = TabsConfig [] (Ripple True)

newtype TabBarConfig =
  TabBarConfig
    { attributes :: [Lucid.Attribute]
    }
  deriving (Show, Eq)

instance Default TabBarConfig where
  def = TabBarConfig []

data TabConfig =
  TabConfig
    { attributes :: [Lucid.Attribute]
    , active     :: Active
    }
  deriving (Show, Eq)

instance Default TabConfig where
  def = TabConfig [] (Active False)

data PanelConfig =
  PanelConfig
    { attributes :: [Lucid.Attribute]
    , active     :: Active
    }
  deriving (Show, Eq)

instance Default PanelConfig where
  def = PanelConfig [] (Active False)
