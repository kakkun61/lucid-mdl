{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Toggles. <https://getmdl.io/components/index.html#toggles-section>
module Lucid.MaterialDesign.Lite.Toggle
  ( checkbox_
  , radio_
  , iconToggle_
  , switch_
  , Config (..)
  , IconToggleConfig (..)
  ) where

import Lucid.MaterialDesign.Lite.Base (HtmlClass (toHtmlClass), Ripple)

import           Data.Default.Class (Default (def))
import           Data.Text          (Text)
import           GHC.Generics       (Generic)
import qualified Lucid

-- | Checkbox. <https://getmdl.io/components/index.html#toggles-section/checkbox>
checkbox_
  :: Monad m
  => Config
  -> Text -- ^ Id.
  -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
checkbox_ (Config ripple checkboxAttributes inputAttributes labelAttributes) id' label =
  Lucid.label_ ([Lucid.classes_ [" mdl-checkbox ", " mdl-js-checkbox ", toHtmlClass ripple], Lucid.for_ id'] ++ checkboxAttributes) $ do
    Lucid.input_ ([Lucid.type_ "checkbox", Lucid.id_ id', Lucid.class_ " mdl-checkbox__input "] ++ inputAttributes)
    Lucid.span_ (Lucid.class_ " mdl-checkbox__label " : labelAttributes) label

-- | Radio button. <https://getmdl.io/components/index.html#toggles-section/radio>
radio_ :: Monad m => Config -> Text -> Text -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
radio_ (Config ripple radioAttributes inputAttributes labelAttributes) id' name label =
  Lucid.label_ ([Lucid.classes_ [" mdl-radio ", " mdl-js-radio ", toHtmlClass ripple], Lucid.for_ id'] ++ radioAttributes) $ do
    Lucid.input_ ([Lucid.type_ "radio", Lucid.id_ id', Lucid.name_ name, Lucid.class_ " mdl-radio__button "] ++ inputAttributes)
    Lucid.span_ (Lucid.class_ " mdl-radio__label " : labelAttributes) label

-- | Icon toggle. <https://getmdl.io/components/index.html#toggles-section/icon-toggle>
iconToggle_ :: Monad m => IconToggleConfig -> Text -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
iconToggle_ (IconToggleConfig ripple iconToggleAttributes inputAttributes) id' icon =
  Lucid.label_ ([Lucid.classes_ [" mdl-icon-toggle ", " mdl-js-icon-toggle ", toHtmlClass ripple], Lucid.for_ id'] ++ iconToggleAttributes) $ do
    Lucid.input_ ([Lucid.type_ "checkbox", Lucid.id_ id', Lucid.class_ " mdl-icon-toggle__input "] ++ inputAttributes)
    Lucid.with icon [Lucid.class_ " mdl-icon-toggle__label "]

-- | Switch. <https://getmdl.io/components/index.html#toggles-section/switch>
switch_ :: Monad m => Config -> Text -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
switch_ (Config ripple switchAttributes inputAttributes labelAttributes) id' label =
  Lucid.label_ ([Lucid.classes_ [" mdl-switch ", " mdl-js-switch ", toHtmlClass ripple], Lucid.for_ id'] ++ switchAttributes) $ do
    Lucid.input_ ([Lucid.type_ "checkbox", Lucid.id_ id', Lucid.class_ " mdl-switch__input "] ++ inputAttributes)
    Lucid.span_ (Lucid.class_ " mdl-switch__label " : labelAttributes) label

data Config =
  Config
    { ripple          :: Ripple
    , attributes      :: [Lucid.Attribute]
    , inputAttributes :: [Lucid.Attribute]
    , labelAttributes :: [Lucid.Attribute]
    }
  deriving (Show, Eq, Generic)

instance Default Config where
  def = Config def [] [] []

data IconToggleConfig =
  IconToggleConfig
    { ripple          :: Ripple
    , attributes      :: [Lucid.Attribute]
    , inputAttributes :: [Lucid.Attribute]
    }
  deriving (Show , Eq, Generic)

instance Default IconToggleConfig where
  def = IconToggleConfig def [] []
