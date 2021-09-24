{-# LANGUAGE OverloadedStrings #-}

module Lucid.MaterialDesign.Icon
  ( head_
  , icon_
  ) where

import qualified Lucid
import Data.Text (Text)

head_ :: Applicative m => Lucid.HtmlT m ()
head_ = Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.href_ "https://fonts.googleapis.com/icon?family=Material+Icons"]

icon_ :: Monad m =>Text -> Lucid.HtmlT m ()
icon_ = Lucid.i_ [Lucid.class_ " material-icons "] . Lucid.toHtml
