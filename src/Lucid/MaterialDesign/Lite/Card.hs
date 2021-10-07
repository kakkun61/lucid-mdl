{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Lucid.MaterialDesign.Lite.Card
  ( card_
  , title_
  , titleText_
  , subtitleText_
  , supportingText_
  , actions_
  , menu_
  , Config (..)
  , InnerConfig (..)
  , Border (..)
  ) where

import Lucid.MaterialDesign.Lite.Base (HtmlClass (toHtmlClass))

import           Data.Default.Class (Default (def))
import           Data.Text          (Text)
import           GHC.Generics       (Generic)
import qualified Lucid

-- | A root container.
card_ :: Applicative m => Config -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
card_ (Config attributes) = Lucid.div_ $ Lucid.class_ " mdl-card " : attributes

-- | A title container, which must be a child of 'card_', which must have 'titleText_' as this child and which may have 'subtitleText_' as this child.
title_ :: Monad m => InnerConfig -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
title_ = inner " mdl-card__title "

-- | A title text container, which must be a child of 'title_'.
titleText_ :: Applicative m => Lucid.HtmlT m () -> Lucid.HtmlT m ()
titleText_ = flip Lucid.with [Lucid.class_ " mdl-card__title-text "]

-- | A subtitle text container, which must be a child of 'title_'.
subtitleText_ :: Applicative m => Lucid.HtmlT m () -> Lucid.HtmlT m ()
subtitleText_ = flip Lucid.with [Lucid.class_ " mdl-card__subtitle-text "]

-- | A supporting text container, which must be a child of 'card_'.
supportingText_ :: Applicative m => InnerConfig -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
supportingText_ = inner " mdl-card__supporting-text "

-- | A actions container, which must be a child of 'card_'.
actions_ :: Applicative m => InnerConfig -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
actions_ = inner " mdl-card__actions "

-- | A menu container, which must be a child of 'card_'.
menu_ :: Applicative m => InnerConfig -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
menu_ = inner " mdl-card__menu "

inner :: Applicative m => Text -> InnerConfig -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
inner mdlClass_ (InnerConfig attributes border) =
  Lucid.div_ $ Lucid.classes_ [mdlClass_, toHtmlClass border] : attributes

newtype Config =
  Config
    { attributes :: [Lucid.Attribute]
    }
  deriving (Show, Eq)

-- |
-- >>> def :: Config
-- Config {attributes = []}
instance Default Config where
  def = Config []

data InnerConfig =
  InnerConfig
    { attributes :: [Lucid.Attribute]
    , border     :: Border
    }
  deriving (Show, Eq)

-- |
-- >>> def :: InnerConfig
-- InnerConfig {attributes = [], border = Border False}
instance Default InnerConfig where
  def = InnerConfig [] def

newtype Border = Border Bool deriving stock (Show, Read, Eq, Ord, Bounded, Generic) deriving newtype Enum

-- |
-- >>> def :: Border
-- Border False
instance Default Border where
  def = Border False

instance HtmlClass Border where
  toHtmlClass (Border True)  = " mdl-card--border "
  toHtmlClass (Border False) = ""
