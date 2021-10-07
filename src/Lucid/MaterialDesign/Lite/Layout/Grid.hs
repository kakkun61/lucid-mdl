{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Lucid.MaterialDesign.Lite.Layout.Grid
  ( grid_
  , cell_
  , GridConfig (..)
  , CellConfig (..)
  , Spacing (..)
  , Column (..)
  , Offset (..)
  , Order (..)
  , Show (..)
  , Stretch (..)
  , Align (..)
  ) where

import Lucid.MaterialDesign.Lite.Base (HtmlClass (toHtmlClass))

import           Data.Default.Class (Default (def))
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           GHC.Generics       (Generic)
import qualified Lucid
import           Prelude            (Applicative, Bool (False, True), Bounded, Enum, Eq, Maybe (Nothing), Num, Ord,
                                     Read, Semigroup ((<>)), Word, maybe, ($))
import qualified Prelude

grid_ :: Applicative m => GridConfig -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
grid_ (GridConfig attributes spacing) =
  Lucid.div_ $ Lucid.classes_ [" mdl-grid ", toHtmlClass spacing] : attributes

data GridConfig =
  GridConfig
    { attributes :: [Lucid.Attribute]
    , spacing    :: Spacing
    }
  deriving (Eq, Prelude.Show)

instance Default GridConfig where
  def = GridConfig [] def

newtype Spacing = Spacing Bool deriving stock (Prelude.Show, Read, Eq, Ord, Bounded, Generic) deriving newtype Enum

instance Default Spacing where
  def = Spacing True

instance HtmlClass Spacing where
  toHtmlClass (Spacing True)  = ""
  toHtmlClass (Spacing False) = " mdl-grid--no-spacing "

cell_ :: Applicative m => CellConfig -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
cell_ (CellConfig attributes column platformColumn offset platformOffset order platformOrder show stretch align) =
  Lucid.div_ $
    Lucid.classes_
      [ " mdl-cell "
      , toHtmlClass column
      , toHtmlClass platformColumn
      , toHtmlClass offset
      , toHtmlClass platformOffset
      , toHtmlClass order
      , toHtmlClass platformOrder
      , toHtmlClass show
      , toHtmlClass stretch
      , toHtmlClass align
      ]
        : attributes

data CellConfig =
  CellConfig
    { attributes     :: [Lucid.Attribute]
    , column         :: Maybe Column
    , platformColumn :: Platform (Maybe Column)
    , offset         :: Maybe Offset
    , platformOffset :: Platform (Maybe Offset)
    , order          :: Maybe Order
    , platformOrder  :: Platform (Maybe Order)
    , show           :: Platform Show
    , stretch        :: Stretch
    , align          :: Maybe Align
    }
  deriving (Prelude.Show, Eq)

instance Default CellConfig where
  def =
    CellConfig
      { attributes     = []
      , column         = Nothing
      , platformColumn = Platform Nothing Nothing Nothing
      , offset         = Nothing
      , platformOffset = Platform Nothing Nothing Nothing
      , order          = Nothing
      , platformOrder  = Platform Nothing Nothing Nothing
      , show           = Platform def def def
      , stretch        = def
      , align          = Nothing
      }

data Platform a =
  Platform
    { desktop :: a
    , tablet  :: a
    , phone   :: a
    }
  deriving (Prelude.Show, Read, Eq, Ord, Generic)

platformToHtmlClass :: (a -> Text) -> Platform a -> Text
platformToHtmlClass f (Platform desktop tablet phone) =
  let
    desktop' = f desktop
    tablet'  = f tablet
    phone'   = f phone
  in
    Text.unwords
      [ if Text.null desktop' then "" else desktop' <> "-desktop"
      , if Text.null tablet' then "" else tablet' <> "-tablet"
      , if Text.null phone' then "" else phone' <> "-phone"
      ]

instance Default a => Default (Platform a) where
  def = Platform def def def

newtype Column = Column Word deriving stock (Eq, Ord, Bounded, Generic) deriving newtype (Prelude.Show, Read, Num)

instance HtmlClass Column where
  toHtmlClass (Column n) = " mdl-cell--" <> Text.pack (Prelude.show n) <> "-col "

instance HtmlClass (Platform (Maybe Column)) where
  toHtmlClass p = platformToHtmlClass (maybe "" $ \n -> " mdl-cell--" <> Text.pack (Prelude.show n) <> "-col ") p

newtype Offset = Offset Word deriving stock (Eq, Ord, Bounded, Generic) deriving newtype (Prelude.Show, Read, Num)

instance HtmlClass Offset where
  toHtmlClass (Offset n) = " mdl-cell--" <> Text.pack (Prelude.show n) <> "-offset "

instance HtmlClass (Platform (Maybe Offset)) where
  toHtmlClass p = platformToHtmlClass (maybe "" $ \n -> " mdl-cell--" <> Text.pack (Prelude.show n) <> "-offset ") p

newtype Order = Order Word deriving stock (Eq, Ord, Bounded, Generic) deriving newtype (Prelude.Show, Read, Num)

instance HtmlClass Order where
  toHtmlClass (Order n) = " mdl-cell--" <> Text.pack (Prelude.show n) <> "-order "

instance HtmlClass (Platform (Maybe Order)) where
  toHtmlClass p = platformToHtmlClass (maybe "" $ \n -> " mdl-cell--" <> Text.pack (Prelude.show n) <> "-order ") p

newtype Show = Show Bool deriving stock (Prelude.Show, Read, Eq, Ord, Generic) deriving newtype Enum

instance Default Show where
  def = Show True

instance HtmlClass (Platform Show) where
  toHtmlClass =
    platformToHtmlClass f
    where
      f (Show False) = " mdl-cell--hide "
      f _            = ""

newtype Stretch = Stretch Bool deriving stock (Prelude.Show, Read, Eq, Ord, Generic) deriving newtype Enum

instance Default Stretch where
  def = Stretch False

instance HtmlClass Stretch where
  toHtmlClass (Stretch True) = " mdl-cell--stretch "
  toHtmlClass _              = ""

data Align
  = Top
  | Middle
  | Bottom
  deriving (Prelude.Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance HtmlClass Align where
  toHtmlClass Top    = " mdl-cell--top "
  toHtmlClass Middle = " mdl-cell--middle "
  toHtmlClass Bottom = " mdl-cell--bottom "
