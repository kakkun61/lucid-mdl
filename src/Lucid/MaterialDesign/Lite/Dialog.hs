{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module Lucid.MaterialDesign.Lite.Dialog
  ( dialog_
  , title_
  , content_
  , actions_
  , Config (..)
  , ActionsConfig (..)
  , FullWidth (..)
  ) where

import qualified Lucid
import qualified Lucid.Base as Lucid
import Data.Default.Class (Default (def))
import GHC.Generics (Generic)
import Lucid.MaterialDesign.Lite.Base (HtmlClass (toHtmlClass))

dialog_ :: Functor m => Config -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
dialog_ (Config attributes) body =
  Lucid.with (Lucid.makeElement "dialog" body) $ Lucid.class_ " mdl-dialog " : attributes

title_ :: Functor m => Lucid.HtmlT m () -> Lucid.HtmlT m ()
title_ = flip Lucid.with [Lucid.class_ " mdl-dialog__title "]

content_ :: Applicative m => Config -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
content_ (Config attributes) =
  Lucid.div_ $ Lucid.class_ " mdl-dialog__content " : attributes

actions_ :: Applicative m => ActionsConfig -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
actions_ (ActionsConfig attributes fullWidth) =
  Lucid.div_ $ Lucid.classes_ [" mdl-dialog__actions ", toHtmlClass fullWidth] : attributes

newtype Config =
  Config
    { attributes :: [Lucid.Attribute]
    }
  deriving (Show, Eq)

instance Default Config where
  def = Config []

data ActionsConfig =
  ActionsConfig
    { attributes :: [Lucid.Attribute]
    , fullWidth :: FullWidth
    }
  deriving (Show, Eq)

instance Default ActionsConfig where
  def = ActionsConfig [] def

newtype FullWidth = FullWidth Bool deriving (Show, Read, Eq, Ord, Bounded, Generic)

instance Default FullWidth where
  def = FullWidth False

instance HtmlClass FullWidth where
  toHtmlClass (FullWidth True) = " mdl-dialog__actions--full-width "
  toHtmlClass (FullWidth False) = ""
