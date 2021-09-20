{-# LANGUAGE OverloadedStrings #-}

module Lucid.Mdl.Slider
  ( slider_
  ) where
import qualified Lucid

slider_ :: Applicative m => [Lucid.Attribute] -> Lucid.HtmlT m ()
slider_ attributes =
  Lucid.input_ ([Lucid.classes_ ["mdl-slider", "mdl-js-slider"], Lucid.type_ "range"] ++ attributes)
