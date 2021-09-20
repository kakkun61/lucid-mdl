{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lucid.Mdl.Chip
  ( chip_
  , button_
  , contact_
  , Deletable (..)
  ) where

import GHC.Generics (Generic)
import qualified Lucid
import Lucid.Mdl.Base (HtmlClass(toHtmlClass))
import Control.Monad (when)
import Data.Maybe (isJust)

data Deletable = Deletable deriving (Show, Read, Eq, Ord, Enum, Generic)

instance HtmlClass Deletable where
  toHtmlClass Deletable = "mdl-chip--deletable"

chip_ :: Monad m => Maybe Deletable -> [Lucid.Attribute] -> [Lucid.Attribute] -> [Lucid.Attribute] -> [Lucid.Attribute] -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
chip_ deletable chipAttributes textAttributes actionAttributes iconAttributes text =
  Lucid.span_ (Lucid.classes_ ["mdl-chip", toHtmlClass deletable] : chipAttributes) $ do
    Lucid.span_ (Lucid.class_ "mdl-chip__text" : textAttributes) text
    when (isJust deletable) $
      Lucid.button_ ([Lucid.type_ "button", Lucid.class_ "mdl-chip__action"] ++ actionAttributes) $
        Lucid.i_ (Lucid.class_ "material-icons" : iconAttributes) "cancel"

button_ :: Applicative m => [Lucid.Attribute] -> [Lucid.Attribute] -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
button_ chipAttributes textAttributes text =
  Lucid.button_ ([Lucid.type_ "button", Lucid.class_ "mdl-chip"] ++ chipAttributes) $ do
    Lucid.span_ (Lucid.class_ "mdl-chip__text" : textAttributes) text

contact_ :: Monad m => Maybe Deletable -> [Lucid.Attribute] -> [Lucid.Attribute] -> [Lucid.Attribute] -> [Lucid.Attribute] -> Lucid.HtmlT m () -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
contact_ deletable chipAttributes textAttributes actionAttributes iconAttributes contact text =
  Lucid.span_ (Lucid.classes_ ["mdl-chip", "mdl-chip--contact", toHtmlClass deletable] : chipAttributes) $ do
    Lucid.with contact [Lucid.class_ " mdl-chip__contact"] -- this space is necessary
    Lucid.span_ (Lucid.class_ "mdl-chip__text" : textAttributes) text
    when (isJust deletable) $
      Lucid.a_ ([Lucid.href_ "#", Lucid.class_ "mdl-chip__action"] ++ actionAttributes) $
        Lucid.i_ (Lucid.class_ "material-icons" : iconAttributes) "cancel"
