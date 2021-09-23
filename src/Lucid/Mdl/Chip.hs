{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lucid.Mdl.Chip
  ( chip_
  , ChipConfig (..)
  , button_
  , ButtonConfig (..)
  , textContact_
  , imageContact_
  , ContactConfig (..)
  , DeletableConfig (..)
  ) where

import           Data.Default.Class (Default (def))
import           Data.Maybe         (isJust)
import           Data.Text          (Text)
import           GHC.Generics       (Generic)
import qualified Lucid

chip_ :: Monad m => ChipConfig -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
chip_ (ChipConfig chipAttributes textAttributes deletable) text =
  Lucid.span_ (Lucid.classes_ (" mdl-chip " : [" mdl-chip--deletable " | isJust deletable]) : chipAttributes) $ do
    Lucid.span_ (Lucid.class_ " mdl-chip__text " : textAttributes) text
    case deletable of
      Just (DeletableConfig actionAttributes iconAttributes) ->
        Lucid.button_ ([Lucid.type_ "button", Lucid.class_ " mdl-chip__action "] ++ actionAttributes) $
          Lucid.i_ (Lucid.class_ " material-icons " : iconAttributes) "cancel"
      Nothing -> pure ()

data ChipConfig =
  ChipConfig
    { chipAttributes :: [Lucid.Attribute]
    , textAttributes :: [Lucid.Attribute]
    , deletable      :: Maybe DeletableConfig
    }
  deriving (Show, Eq, Generic)

instance Default ChipConfig where
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

textContact_ :: Monad m => ContactConfig -> Lucid.HtmlT m () -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
textContact_ config contact = contact_ config $ Lucid.span_ contact

imageContact_ :: Monad m => ContactConfig -> Text -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
imageContact_ config image = contact_ config $ Lucid.img_ [Lucid.src_ image]

contact_ :: Monad m => ContactConfig -> Lucid.HtmlT m () -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
contact_ (ContactConfig contactAttributes chipAttributes textAttributes deletable) contact text =
  Lucid.span_ (Lucid.classes_ ([" mdl-chip ", " mdl-chip--contact "] ++ [" mdl-chip--deletable " | isJust deletable]) : chipAttributes) $ do
    Lucid.with contact (Lucid.class_ " mdl-chip__contact " : contactAttributes)
    Lucid.span_ (Lucid.class_ " mdl-chip__text " : textAttributes) text
    case deletable of
      Just (DeletableConfig actionAttributes iconAttributes) ->
        Lucid.a_ (Lucid.class_ " mdl-chip__action " : actionAttributes) $
          Lucid.i_ (Lucid.class_ "material-icons " : iconAttributes) "cancel"
      Nothing -> pure ()

data ContactConfig =
  ContactConfig
    { contactAttributes :: [Lucid.Attribute]
    , chipAttributes    :: [Lucid.Attribute]
    , textAttributes    :: [Lucid.Attribute]
    , deletable         :: Maybe DeletableConfig
    }
  deriving (Show, Eq, Generic)

instance Default ContactConfig where
  def = ContactConfig [] [] [] Nothing

data DeletableConfig =
  DeletableConfig
    { actionAttributes :: [Lucid.Attribute]
    , iconAttributes   :: [Lucid.Attribute]
    }
  deriving (Show, Eq, Generic)

instance Default DeletableConfig where
  def = DeletableConfig [] []
