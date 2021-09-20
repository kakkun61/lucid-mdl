{-# LANGUAGE OverloadedStrings #-}

module Lucid.Mdl.TextField
  ( textField_
  ) where

import qualified Lucid
import Data.Text (Text)

textField_ :: Monad m => Text -> [Lucid.Attribute] -> [Lucid.Attribute] -> [Lucid.Attribute] -> Lucid.HtmlT m () -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
textField_ id' textFieldAttributes inputAttributes labelAttributes label error =
  Lucid.div_ (Lucid.classes_ ["mdl-textfield", "mdl-js-textfield"] : textFieldAttributes) $ do
    Lucid.input_ ([Lucid.class_ "mdl-textfield__input", Lucid.type_ "text", Lucid.id_ id'] ++ inputAttributes)
    Lucid.label_ ([Lucid.class_ "mdl-textfield__label", Lucid.for_ id'] ++ labelAttributes) label
    
