{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lucid.MaterialDesign.Lite.Head
  ( script_
  , css_
  , Color (..)
  ) where

import           Data.Text    (Text)
import           GHC.Generics (Generic)
import qualified Lucid

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

css_ :: Monad m => Color -> Color -> Lucid.HtmlT m ()
css_ primaryColor accentColor = do
  Lucid.link_
    [ Lucid.rel_ "stylesheet"
    , Lucid.href_ $ "https://code.getmdl.io/1.3.0/material." <> colorToUrl primaryColor <> "-" <> colorToUrl accentColor <> ".min.css"
    ]
  -- workaround https://github.com/google/material-design-lite/issues/1002#issuecomment-170718247
  Lucid.style_
    ".mdl-card__title {\
    \  flex-direction: column;\
    \  justify-content: flex-end;\
    \}\
    \.mdl-card__title-text, .mdl-card__subtitle-text {\
    \  align-self: flex-start;\
    \}"

script_ :: Monad m => Lucid.HtmlT m ()
script_ = Lucid.script_ [Lucid.defer_ "", Lucid.src_ "https://code.getmdl.io/1.3.0/material.min.js"] ("" :: Lucid.Html ())
