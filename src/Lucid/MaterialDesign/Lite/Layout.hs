{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lucid.MaterialDesign.Lite.Layout
  ( layout_
  , header_
  , headerRow_
  , title_
  , navigation_
  , navigationLink_
  , drawer_
  , content_
  , spacer_
  , Config (..)
  , LayoutConfig (..)
  , HeaderConfig (..)
  , FixedDrawer (..)
  , FixedHeader (..)
  , Transparent (..)
  ) where

import qualified Lucid
import GHC.Generics (Generic)
import Data.Default.Class (Default (def))
import Lucid.MaterialDesign.Lite.Base (HtmlClass (toHtmlClass))

layout_ :: Applicative m => LayoutConfig -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
layout_ (LayoutConfig attributes fixedDrawer fixedHeader) =
  Lucid.div_ $ Lucid.classes_ [" mdl-layout ", " mdl-js-layout ", toHtmlClass fixedDrawer, toHtmlClass fixedHeader] : attributes

header_ :: Applicative m => HeaderConfig -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
header_ (HeaderConfig attributes transparent) =
  Lucid.header_ $ Lucid.classes_ [" mdl-layout__header ", toHtmlClass transparent] : attributes

headerRow_ :: Applicative m => Config -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
headerRow_ (Config attributes) =
  Lucid.div_ $ Lucid.class_ " mdl-layout__header-row " : attributes

title_ :: Applicative m => Config -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
title_ (Config attributes) =
  Lucid.span_ $ Lucid.class_ " mdl-layout-title " : attributes

navigation_ :: Applicative m => Config -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
navigation_ (Config attributes) =
  Lucid.nav_ $ Lucid.class_ " mdl-navigation " : attributes

navigationLink_ :: Applicative m => Config -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
navigationLink_ (Config attributes) =
  Lucid.a_ $ Lucid.class_ " mdl-navigation__link " : attributes

drawer_ :: Applicative m => Config -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
drawer_ (Config attributes) =
  Lucid.div_ $ Lucid.class_ " mdl-layout__drawer " : attributes

content_ :: Applicative m => Config -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
content_ (Config attributes) =
  Lucid.main_ $ Lucid.class_ " mdl-layout__content " : attributes

spacer_ :: Applicative m => Lucid.HtmlT m ()
spacer_ = Lucid.div_ [Lucid.class_ " mdl-layout-spacer "] mempty

newtype Config =
  Config
    { attributes :: [Lucid.Attribute]
    }
  deriving (Show, Eq)

instance Default Config where
  def = Config []

data LayoutConfig =
  LayoutConfig
    { attributes :: [Lucid.Attribute]
    , fixedDrawer :: FixedDrawer
    , fixedHeader :: FixedHeader
    }
  deriving (Show, Eq)

instance Default LayoutConfig where
  def = LayoutConfig [] def def

data HeaderConfig =
  HeaderConfig
    { attributes :: [Lucid.Attribute]
    , transparent :: Transparent
    }
  deriving (Show, Eq)

instance Default HeaderConfig where
  def = HeaderConfig [] def

newtype FixedDrawer = FixedDrawer Bool deriving (Show, Read, Eq, Ord, Bounded, Generic)

instance Default FixedDrawer where
  def = FixedDrawer False

instance HtmlClass FixedDrawer where
  toHtmlClass (FixedDrawer True) = " mdl-layout--fixed-drawer "
  toHtmlClass (FixedDrawer False) = ""

newtype FixedHeader = FixedHeader Bool deriving (Show, Read, Eq, Ord, Bounded, Generic)

instance Default FixedHeader where
  def = FixedHeader False

instance HtmlClass FixedHeader where
  toHtmlClass (FixedHeader True) = " mdl-layout--fixed-header "
  toHtmlClass (FixedHeader False) = ""

newtype Transparent = Transparent Bool deriving (Show, Read, Eq, Ord, Bounded, Generic)

instance Default Transparent where
  def = Transparent False

instance HtmlClass Transparent where
  toHtmlClass (Transparent True) = " mdl-layout__header--transparent "
  toHtmlClass (Transparent False) = ""
