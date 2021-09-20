{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lucid.Mdl.Base
  ( Ripple (..)
  , HtmlClass (..)
  ) where

import Data.Text    (Text)
import GHC.Generics (Generic)

data Ripple = Ripple deriving (Show, Read, Eq, Ord, Enum, Generic)

class HtmlClass a where
  toHtmlClass :: a -> Text

instance HtmlClass a => HtmlClass (Maybe a) where
  toHtmlClass Nothing  = ""
  toHtmlClass (Just a) = toHtmlClass a

instance HtmlClass Ripple where
  toHtmlClass _ = "mdl-js-ripple-effect"
