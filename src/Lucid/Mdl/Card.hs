{-# LANGUAGE OverloadedStrings #-}

module Lucid.Mdl.Card
  ( card_
  , Config (..)
  ) where

import qualified Lucid
import Data.Default.Class (Default(def))

card_ :: Monad m => Config -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
card_ (Config cardAttributes) title =
  Lucid.div_ ([Lucid.class_ " mdl-card "] ++ cardAttributes) $ do
    Lucid.div_ [Lucid.class_ "mdl-card__title"] $ do
      Lucid.with title [Lucid.class_ " mdl-card__title-text"]

data Config =
  Config
    { cardAttributes :: [Lucid.Attribute]
    }

instance Default Config where
  def = Config []
