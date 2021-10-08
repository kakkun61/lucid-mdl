{-# LANGUAGE OverloadedStrings #-}

module Lucid.MaterialDesign.Lite.Layout.Footer.Mega
  ( footer_
  , topSection_
  , middleSection_
  , bottomSection_
  , leftSection_
  , rightSection_
  , socialButton_
  , dropDownSection_
  , heading_
  , linkList_
  , Config (..)
  ) where

import           Data.Default.Class (Default (def))
import           Data.Text          (Text)
import qualified Data.Text          as Text
import qualified Lucid

footer_ :: Applicative m => Config -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
footer_ = section "" Lucid.footer_

topSection_ :: Applicative m => Config -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
topSection_ = section "top-section" Lucid.div_

middleSection_ :: Applicative m => Config -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
middleSection_ = section "middle-section" Lucid.div_

bottomSection_ :: Applicative m => Config -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
bottomSection_ = section "bottom-section" Lucid.div_

leftSection_ :: Applicative m => Config -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
leftSection_ = section "left-section" Lucid.div_

rightSection_ :: Applicative m => Config -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
rightSection_ = section "right-section" Lucid.div_

socialButton_ :: Applicative m => Config -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
socialButton_ = section "social-btn" Lucid.button_

dropDownSection_ :: Applicative m => Config -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
dropDownSection_ = section "drop-down-section" Lucid.div_

heading_ :: Applicative m => Config -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
heading_ = section "heading" Lucid.h1_

linkList_ :: Applicative m => Config -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
linkList_ = section "link-list" Lucid.ul_

section :: Text -> ([Lucid.Attribute] -> Lucid.HtmlT m () -> Lucid.HtmlT m ()) -> Config -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
section class_ section' (Config attributes) =
  section' $ Lucid.class_ (" mdl-mega-footer" <> (if Text.null class_ then "" else "__" <> class_) <> " ") : attributes

newtype Config =
  Config
    { attributes :: [Lucid.Attribute]
    }
  deriving (Show, Eq)

instance Default Config where
  def = Config []
