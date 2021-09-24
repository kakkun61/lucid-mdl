{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Lucid.MaterialDesign.Lite.Chip
  ( chip_
  , ChipConfig (..)
  , button_
  , ButtonConfig (..)
  , textContact_
  , imageContact_
  , ContactConfig (..)
  , DeletableConfig (..)
  ) where

import qualified Lucid.MaterialDesign.Icon as Icon

import           Data.Default.Class (Default (def))
import           Data.Maybe         (isJust)
import           Data.Text          (Text)
import           GHC.Generics       (Generic)
import qualified Lucid
import Data.Functor.Identity (Identity)

chip_ :: Monad m => ChipConfig m -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
chip_ (ChipConfig chipAttributes textAttributes deletable) text =
  Lucid.span_ (Lucid.classes_ (" mdl-chip " : [" mdl-chip--deletable " | isJust deletable]) : chipAttributes) $ do
    Lucid.span_ (Lucid.class_ " mdl-chip__text " : textAttributes) text
    case deletable of
      Just (DeletableConfig actionAttributes icon) ->
        Lucid.button_ ([Lucid.type_ "button", Lucid.class_ " mdl-chip__action "] ++ actionAttributes) icon
      Nothing -> pure ()

data ChipConfig m =
  ChipConfig
    { chipAttributes :: [Lucid.Attribute]
    , textAttributes :: [Lucid.Attribute]
    , deletable      :: Maybe (DeletableConfig m)
    }
  deriving Generic

deriving instance Show (ChipConfig Identity)

instance Default (ChipConfig m) where
  def = ChipConfig [] [] Nothing

button_ :: Applicative m => ButtonConfig -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
button_ (ButtonConfig chipAttributes textAttributes) text =
  Lucid.button_ ([Lucid.type_ "button", Lucid.class_ " mdl-chip "] ++ chipAttributes) $ do
    Lucid.span_ (Lucid.class_ " mdl-chip__text " : textAttributes) text

data ButtonConfig =
  ButtonConfig
    { chipAttributes :: [Lucid.Attribute]
    , textAttributes :: [Lucid.Attribute]
    }
  deriving (Show, Eq, Generic)

instance Default ButtonConfig where
  def = ButtonConfig [] []

textContact_ :: Monad m => ContactConfig m -> Lucid.HtmlT m () -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
textContact_ config contact = contact_ config $ Lucid.span_ contact

imageContact_ :: Monad m => ContactConfig m -> Text -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
imageContact_ config image = contact_ config $ Lucid.img_ [Lucid.src_ image]

contact_ :: Monad m => ContactConfig m -> Lucid.HtmlT m () -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
contact_ (ContactConfig contactAttributes chipAttributes textAttributes deletable) contact text =
  Lucid.span_ (Lucid.classes_ ([" mdl-chip ", " mdl-chip--contact "] ++ [" mdl-chip--deletable " | isJust deletable]) : chipAttributes) $ do
    Lucid.with contact (Lucid.class_ " mdl-chip__contact " : contactAttributes)
    Lucid.span_ (Lucid.class_ " mdl-chip__text " : textAttributes) text
    case deletable of
      Just (DeletableConfig actionAttributes icon) ->
        Lucid.a_ (Lucid.class_ " mdl-chip__action " : actionAttributes) icon
      Nothing -> pure ()

data (ContactConfig m) =
  ContactConfig
    { contactAttributes :: [Lucid.Attribute]
    , chipAttributes    :: [Lucid.Attribute]
    , textAttributes    :: [Lucid.Attribute]
    , deletable         :: Maybe (DeletableConfig m)
    }
  deriving Generic

deriving instance Show (ContactConfig Identity)

instance Default (ContactConfig m) where
  def = ContactConfig [] [] [] Nothing

data DeletableConfig m =
  DeletableConfig
    { actionAttributes :: [Lucid.Attribute]
    , icon :: Lucid.HtmlT m ()
    }
  deriving Generic

deriving instance Show (DeletableConfig Identity)

instance Monad m => Default (DeletableConfig m) where
  def = DeletableConfig [] (Icon.icon_ "cancel")
