{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Lucid.MaterialDesign.Lite.Layout.Navigation
  ( layout_
  , header_
  , headerRow_
  , title_
  , navigation_
  , navigationLink_
  , drawer_
  , tabBar_
  , tab_
  , tabPanel_
  , icon_
  , content_
  , spacer_
  , Config (..)
  , LayoutConfig (..)
  , HeaderConfig (..)
  , FixedDrawer (..)
  , FixedHeader (..)
  , HeaderTransparent (..)
  , TabBarConfig (..)
  , TabConfig (..)
  , TabPanelConfig (..)
  ) where

import Lucid.MaterialDesign.Lite.Base (Active, Flag (Flag), HtmlClass (toHtmlClass), Ripple)

import           Data.Default.Class (Default (def))
import           GHC.Generics       (Generic)
import qualified Lucid

layout_ :: Applicative m => LayoutConfig -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
layout_ (LayoutConfig attributes fixedDrawer fixedHeader scrollingHeader waterfallHeader fixedTabs drawerButton) =
  Lucid.div_ $
    Lucid.classes_
      [ " mdl-layout "
      , " mdl-js-layout "
      , toHtmlClass fixedDrawer
      , toHtmlClass fixedHeader
      , toHtmlClass scrollingHeader
      , toHtmlClass waterfallHeader
      , toHtmlClass fixedTabs
      , toHtmlClass drawerButton
      ]
        : attributes

header_ :: Applicative m => HeaderConfig -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
header_ (HeaderConfig attributes transparent seamed) =
  Lucid.header_ $ Lucid.classes_ [" mdl-layout__header ", toHtmlClass transparent, toHtmlClass seamed] : attributes

headerRow_ :: Applicative m => Config -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
headerRow_ (Config attributes largeScreenOnly smallScreenOnly) =
  Lucid.div_ $ Lucid.classes_ [" mdl-layout__header-row ", toHtmlClass largeScreenOnly, toHtmlClass smallScreenOnly] : attributes

title_ :: Applicative m => Config -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
title_ (Config attributes largeScreenOnly smallScreenOnly) =
  Lucid.span_ $ Lucid.classes_ [" mdl-layout-title ", toHtmlClass largeScreenOnly, toHtmlClass smallScreenOnly] : attributes

navigation_ :: Applicative m => Config -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
navigation_ (Config attributes largeScreenOnly smallScreenOnly) =
  Lucid.nav_ $ Lucid.classes_ [" mdl-navigation ", toHtmlClass largeScreenOnly, toHtmlClass smallScreenOnly] : attributes

navigationLink_ :: Applicative m => Config -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
navigationLink_ (Config attributes largeScreenOnly smallScreenOnly) =
  Lucid.a_ $ Lucid.classes_ [" mdl-navigation__link ", toHtmlClass largeScreenOnly, toHtmlClass smallScreenOnly] : attributes

drawer_ :: Applicative m => Config -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
drawer_ (Config attributes largeScreenOnly smallScreenOnly) =
  Lucid.div_ $ Lucid.classes_ [" mdl-layout__drawer ", toHtmlClass largeScreenOnly, toHtmlClass smallScreenOnly] : attributes

tabBar_ :: Applicative m => TabBarConfig -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
tabBar_ (TabBarConfig attributes manualSwitch ripple) =
  Lucid.div_ $ Lucid.classes_ [" mdl-layout__tab-bar ", toHtmlClass manualSwitch, toHtmlClass ripple] : attributes

tab_ :: Applicative m => TabConfig -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
tab_ (TabConfig attributes active) =
  Lucid.a_ $ Lucid.classes_ [" mdl-layout__tab ", toHtmlClass active] : attributes

tabPanel_ :: Applicative m => TabPanelConfig -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
tabPanel_ (TabPanelConfig attributes active) =
  Lucid.section_ $ Lucid.classes_ [" mdl-layout__tab-panel ", toHtmlClass active] : attributes

icon_ :: Applicative m => Lucid.HtmlT m ()
icon_ = Lucid.div_ [Lucid.class_ " mdl-layout-icon "] mempty

content_ :: Applicative m => Config -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
content_ (Config attributes largeScreenOnly smallScreenOnly) =
  Lucid.main_ $ Lucid.classes_ [" mdl-layout__content ", toHtmlClass largeScreenOnly, toHtmlClass smallScreenOnly] : attributes

spacer_ :: Applicative m => Lucid.HtmlT m ()
spacer_ = Lucid.div_ [Lucid.class_ " mdl-layout-spacer "] mempty

data Config =
  Config
    { attributes      :: [Lucid.Attribute]
    , largeScreenOnly :: LargeScreenOnly
    , smallScreenOnly :: SmallScreenOnly
    }
  deriving (Show, Eq)

instance Default Config where
  def = Config [] def def

data LayoutConfig =
  LayoutConfig
    { attributes      :: [Lucid.Attribute]
    , fixedDrawer     :: FixedDrawer
    , fixedHeader     :: FixedHeader
    , scrollingHeader :: ScrollingHeader
    , waterfall       :: WaterfallHeader
    , fixedTabs       :: FixedTabs
    , drawerButton    :: DrawerButton
    }
  deriving (Show, Eq)

instance Default LayoutConfig where
  def = LayoutConfig [] def def def def def def

data HeaderConfig =
  HeaderConfig
    { attributes  :: [Lucid.Attribute]
    , transparent :: HeaderTransparent
    , seamed      :: HeaderSeamed
    }
  deriving (Show, Eq)

instance Default HeaderConfig where
  def = HeaderConfig [] def def

newtype FixedDrawer = FixedDrawer Bool deriving stock (Show, Read, Eq, Ord, Bounded, Generic) deriving newtype Enum deriving Default via Flag

instance HtmlClass FixedDrawer where
  toHtmlClass (FixedDrawer True)  = " mdl-layout--fixed-drawer "
  toHtmlClass (FixedDrawer False) = ""

newtype FixedHeader = FixedHeader Bool deriving stock (Show, Read, Eq, Ord, Bounded, Generic) deriving newtype Enum deriving Default via Flag

instance HtmlClass FixedHeader where
  toHtmlClass (FixedHeader True)  = " mdl-layout--fixed-header "
  toHtmlClass (FixedHeader False) = ""

newtype HeaderTransparent = HeaderTransparent Bool deriving stock (Show, Read, Eq, Ord, Bounded, Generic) deriving newtype Enum deriving Default via Flag

instance HtmlClass HeaderTransparent where
  toHtmlClass (HeaderTransparent True)  = " mdl-layout__header--transparent "
  toHtmlClass (HeaderTransparent False) = ""

newtype HeaderSeamed = HeaderSeamed Bool deriving stock (Show, Read, Eq, Ord, Bounded, Generic) deriving newtype Enum deriving Default via Flag

instance HtmlClass HeaderSeamed where
  toHtmlClass (HeaderSeamed True)  = " mdl-layout__header--seamed "
  toHtmlClass (HeaderSeamed False) = ""

newtype ScrollingHeader = ScrollingHeader Bool deriving stock (Show, Read, Eq, Ord, Bounded, Generic) deriving newtype Enum deriving Default via Flag

instance HtmlClass ScrollingHeader where
  toHtmlClass (ScrollingHeader True)  = " mdl-layout__header--scroll "
  toHtmlClass (ScrollingHeader False) = ""

newtype WaterfallHeader = WaterfallHeader Bool deriving stock (Show, Read, Eq, Ord, Bounded, Generic) deriving newtype Enum deriving Default via Flag

instance HtmlClass WaterfallHeader where
  toHtmlClass (WaterfallHeader True)  = " mdl-layout--header--waterfall "
  toHtmlClass (WaterfallHeader False) = ""

data TabBarConfig =
  TabBarConfig
    { attributes   :: [Lucid.Attribute]
    , manualSwitch :: TabManualSwitch
    , ripple       :: Ripple
    }
  deriving (Show, Eq)

instance Default TabBarConfig where
  def = TabBarConfig [] def def

data TabConfig =
  TabConfig
    { attributes :: [Lucid.Attribute]
    , active     :: Active
    }
  deriving (Show, Eq)

instance Default TabConfig where
  def = TabConfig [] def

newtype TabManualSwitch = TabManualSwitch Bool deriving stock (Show, Read, Eq, Ord, Bounded, Generic) deriving newtype Enum deriving Default via Flag

instance HtmlClass TabManualSwitch where
  toHtmlClass (TabManualSwitch True)  = " mdl-layout__tab-manual-switch "
  toHtmlClass (TabManualSwitch False) = ""

data TabPanelConfig =
  TabPanelConfig
    { attributes :: [Lucid.Attribute]
    , active     :: Active
    }
  deriving (Show, Eq)

instance Default TabPanelConfig where
  def = TabPanelConfig [] def

newtype FixedTabs = FixedTabs Bool deriving stock (Show, Read, Eq, Ord, Bounded, Generic) deriving newtype Enum deriving Default via Flag

instance HtmlClass FixedTabs where
  toHtmlClass (FixedTabs True)  = " mdl-layout--fixed-tabs "
  toHtmlClass (FixedTabs False) = ""

data DrawerButton
  = Show
  | Hide
  | HideDesktop
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

instance Default DrawerButton where
  def = Show

instance HtmlClass DrawerButton where
  toHtmlClass Show        = ""
  toHtmlClass Hide        = " mdl-layout--no-drawer-button "
  toHtmlClass HideDesktop = " mdl-layout--no-desktop-drawer-button "

newtype LargeScreenOnly = LargeScreenOnly Bool deriving stock (Show, Read, Eq, Ord, Bounded, Generic) deriving newtype Enum deriving Default via Flag

instance HtmlClass LargeScreenOnly where
  toHtmlClass (LargeScreenOnly True)  = " mdl-layout--large-screen-only "
  toHtmlClass (LargeScreenOnly False) = ""

newtype SmallScreenOnly = SmallScreenOnly Bool deriving stock (Show, Read, Eq, Ord, Bounded, Generic) deriving newtype Enum deriving Default via Flag

instance HtmlClass SmallScreenOnly where
  toHtmlClass (SmallScreenOnly True)  = " mdl-layout--small-screen-only "
  toHtmlClass (SmallScreenOnly False) = ""
