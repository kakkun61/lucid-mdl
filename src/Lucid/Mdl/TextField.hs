{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Lucid.Mdl.TextField
  ( textField_
  , textArea_
  , Config (..)
  , FloatingLabel (..)
  , ErrorConfig (..)
  ) where

import           Data.Default.Class    (Default (def))
import           Data.Functor.Identity (Identity)
import           Data.Maybe            (maybeToList)
import           Data.Text             (Text)
import           GHC.Generics          (Generic)
import qualified Lucid
import           Lucid.Mdl.Base        (HtmlClass (toHtmlClass))

textField_ :: Monad m => Config m -> Text -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
textField_ = component Lucid.input_

textArea_ :: Monad m => Config m -> Text -> Lucid.HtmlT m () -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
textArea_ config id' label input = component (flip Lucid.textarea_ input) config id' label

component :: Monad m => ([Lucid.Attribute] -> Lucid.HtmlT m ()) -> Config m -> Text -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
component input (Config floatingLabel textFieldAttributes inputAttributes labelAttributes error) id' label =
  Lucid.div_ (Lucid.classes_ ([" mdl-textfield ", " mdl-js-textfield "] ++ [toHtmlClass floatingLabel]) : textFieldAttributes) $ do
    input $ [Lucid.class_ " mdl-textfield__input ", Lucid.type_ "text", Lucid.id_ id'] ++ maybeToList (Lucid.pattern_ . pattern <$> error) ++ inputAttributes
    Lucid.label_ ([Lucid.class_ " mdl-textfield__label ", Lucid.for_ id'] ++ labelAttributes) label
    case error of
      Just ErrorConfig { error, attributes } ->
        Lucid.span_ (Lucid.class_ " mdl-textfield__error " : attributes) error
      Nothing -> pure ()

data Config m =
  Config
    { floatingLabel       :: Maybe FloatingLabel
    , textFieldAttributes :: [Lucid.Attribute]
    , inputAttributes     :: [Lucid.Attribute]
    , labelAttributes     :: [Lucid.Attribute]
    , error               :: Maybe (ErrorConfig m)
    }
  deriving Generic

deriving instance Show (Config Identity)

instance Default (Config m) where
  def = Config Nothing [] [] [] Nothing

data FloatingLabel = FloatingLabel deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance HtmlClass FloatingLabel where
  toHtmlClass _ = " mdl-textfield--floating-label "

data ErrorConfig m =
  ErrorConfig
    { pattern    :: Text
    , attributes :: [Lucid.Attribute]
    , error      :: Lucid.HtmlT m ()
    }
  deriving Generic

deriving instance Show (ErrorConfig Identity)
