{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lucid.Mdl.Head
  ( script_
  , css_
  , font_
  , Color (..)
  ) where

import           Data.Text    (Text)
import           GHC.Generics (Generic)
import qualified Lucid

font_ :: Applicative m => Lucid.HtmlT m ()
font_ = Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.href_ "https://fonts.googleapis.com/icon?family=Material+Icons"]

data Color
  = DeepOrange
  | Red
  | Pink
  | Purple
  | DeepPurple
  | Indigo
  | Blue
  | LightBlue
  | Cyan
  | Teal
  | Green
  | LightGreen
  | Lime
  | Yellow
  | Amber
  | Orange
  | Brown
  | BlueGrey
  | Grey
  deriving (Show, Read, Eq, Ord, Enum, Generic)

colorToUrl :: Color -> Text
colorToUrl DeepOrange = "deep_orange"
colorToUrl Red        = "red"
colorToUrl Pink       = "pink"
colorToUrl Purple     = "purple"
colorToUrl DeepPurple = "deep_purple"
colorToUrl Indigo     = "indigo"
colorToUrl Blue       = "blue"
colorToUrl LightBlue  = "light_blue"
colorToUrl Cyan       = "cyan"
colorToUrl Teal       = "teal"
colorToUrl Green      = "green"
colorToUrl LightGreen = "light_green"
colorToUrl Lime       = "lime"
colorToUrl Yellow     = "yellow"
colorToUrl Amber      = "amber"
colorToUrl Orange     = "orange"
colorToUrl Brown      = "brown"
colorToUrl BlueGrey   = "blue_grey"
colorToUrl Grey       = "grey"

css_ :: Applicative m => Color -> Color -> Lucid.HtmlT m ()
css_ primaryColor accentColor =
  Lucid.link_
    [ Lucid.rel_ "stylesheet"
    , Lucid.href_ $ "https://code.getmdl.io/1.3.0/material." <> colorToUrl primaryColor <> "-" <> colorToUrl accentColor <> ".min.css"
    ]

script_ :: Monad m => Lucid.HtmlT m ()
script_ = Lucid.script_ [Lucid.defer_ "", Lucid.src_ "https://code.getmdl.io/1.3.0/material.min.js"] ("" :: Lucid.Html ())
