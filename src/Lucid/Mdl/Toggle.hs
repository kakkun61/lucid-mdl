{-# LANGUAGE OverloadedStrings #-}

-- | Toggles. <https://getmdl.io/components/index.html#toggles-section>
module Lucid.Mdl.Toggle
  ( checkbox_
  , radio_
  , iconToggle_
  , switch_
  ) where

import Lucid.Mdl.Base (HtmlClass (toHtmlClass), Ripple)

import           Data.Text (Text)
import qualified Lucid

-- | Checkbox. <https://getmdl.io/components/index.html#toggles-section/checkbox>
checkbox_ :: Monad m => Text -> Maybe Ripple -> [Lucid.Attribute] -> [Lucid.Attribute] -> [Lucid.Attribute] -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
checkbox_ id' ripple checkboxAttributes inputAttributes labelAttributes label =
  Lucid.label_ ([Lucid.classes_ ["mdl-checkbox", "mdl-js-checkbox", toHtmlClass ripple], Lucid.for_ id'] ++ checkboxAttributes) $ do
    Lucid.input_ ([Lucid.type_ "checkbox", Lucid.id_ id', Lucid.class_ "mdl-checkbox__input"] ++ inputAttributes)
    Lucid.span_ (Lucid.class_ "mdl-checkbox__label" : labelAttributes) label

-- | Radio button. <https://getmdl.io/components/index.html#toggles-section/radio>
radio_ :: Monad m => Text -> Text -> Maybe Ripple -> [Lucid.Attribute] -> [Lucid.Attribute] -> [Lucid.Attribute] -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
radio_ id' name ripple radioAttributes inputAttributes labelAttributes label =
  Lucid.label_ ([Lucid.classes_ ["mdl-radio", "mdl-js-radio", toHtmlClass ripple], Lucid.for_ id'] ++ radioAttributes) $ do
    Lucid.input_ ([Lucid.type_ "radio", Lucid.id_ id', Lucid.name_ name, Lucid.class_ "mdl-radio__button"] ++ inputAttributes)
    Lucid.span_ (Lucid.class_ "mdl-radio__label" : labelAttributes) label

-- | Icon toggle. <https://getmdl.io/components/index.html#toggles-section/icon-toggle>
iconToggle_ :: Monad m => Text -> Maybe Ripple -> [Lucid.Attribute] -> [Lucid.Attribute] -> [Lucid.Attribute] -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
iconToggle_ id' ripple iconToggleAttributes inputAttributes labelAttributes label =
  Lucid.label_ ([Lucid.classes_ ["mdl-icon-toggle", "mdl-js-icon-toggle", toHtmlClass ripple], Lucid.for_ id'] ++ iconToggleAttributes) $ do
    Lucid.input_ ([Lucid.type_ "checkbox", Lucid.id_ id', Lucid.class_ "mdl-icon-toggle__input"] ++ inputAttributes)
    Lucid.span_ (Lucid.classes_ ["mdl-icon-toggle__label", "material-icons"] : labelAttributes) label

-- | Switch. <https://getmdl.io/components/index.html#toggles-section/switch>
switch_ :: Monad m => Text -> Maybe Ripple -> [Lucid.Attribute] -> [Lucid.Attribute] -> [Lucid.Attribute] -> Lucid.HtmlT m ()
switch_ id' ripple switchAttributes inputAttributes labelAttributes =
  Lucid.label_ ([Lucid.classes_ ["mdl-switch", "mdl-js-switch", toHtmlClass ripple], Lucid.for_ id'] ++ switchAttributes) $ do
    Lucid.input_ ([Lucid.type_ "checkbox", Lucid.id_ id', Lucid.class_ "mdl-switch__input"] ++ inputAttributes)
    Lucid.span_ (Lucid.class_ "mdl-switch__label" : labelAttributes) ""
