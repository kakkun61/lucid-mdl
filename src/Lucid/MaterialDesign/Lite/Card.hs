{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

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

import qualified Lucid
import GHC.Generics (Generic)
import Lucid.MaterialDesign.Lite.Base (HtmlClass(toHtmlClass))
import Data.Default.Class (Default (def))
import Data.Text (Text)

card_ :: Applicative m => Config -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
card_ (Config attributes) = Lucid.div_ (Lucid.class_ " mdl-card " : attributes)

title_ :: Monad m => InnerConfig -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
title_ = inner " mdl-card__title "

titleText_ :: Applicative m => Lucid.HtmlT m () -> Lucid.HtmlT m ()
titleText_ = flip Lucid.with [Lucid.class_ " mdl-card__title-text "]

subtitleText_ :: Applicative m => Lucid.HtmlT m () -> Lucid.HtmlT m ()
subtitleText_ = flip Lucid.with [Lucid.class_ " mdl-card__subtitle-text "]

supportingText_ :: Applicative m => InnerConfig -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
supportingText_ = inner " mdl-card__supporting-text "

actions_ :: Applicative m => InnerConfig -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
actions_ = inner " mdl-card__actions "

menu_ :: Applicative m => InnerConfig -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
menu_ = inner " mdl-card__menu "

inner :: Applicative m => Text -> InnerConfig -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
inner mdlClass_ (InnerConfig attributes border) =
  Lucid.div_ (Lucid.classes_ [mdlClass_, toHtmlClass border] : attributes)

newtype Config =
  Config
    { attributes :: [Lucid.Attribute]
    }
  deriving (Show, Eq)

instance Default Config where
  def = Config []

data InnerConfig =
  InnerConfig
    { attributes :: [Lucid.Attribute]
    , border :: Border
    }
  deriving (Show, Eq)

instance Default InnerConfig where
  def = InnerConfig [] (Border False)

newtype Border = Border Bool deriving (Show, Read, Eq, Ord, Bounded, Generic)

instance HtmlClass Border where
  toHtmlClass (Border True) = " mdl-card--border "
  toHtmlClass (Border False) = ""
