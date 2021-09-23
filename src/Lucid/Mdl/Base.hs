{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Lucid.Mdl.Base
  ( Ripple (..)
  , Color (..)
  , TextColor (..)
  , spacer_
  , HtmlClass (..)
  ) where

import Data.Text    (Text)
import GHC.Generics (Generic)
import qualified Lucid

newtype Ripple = Ripple Bool deriving (Show, Read, Eq, Ord, Enum, Generic)

instance HtmlClass Ripple where
  toHtmlClass (Ripple True) = " mdl-js-ripple-effect "
  toHtmlClass (Ripple False) = ""

data Color
  = Red100
  | Red200
  | Red300
  | Red400
  | Red500
  | Red600
  | Red700
  | Red800
  | Red900
  | RedAccent100
  | RedAccent200
  | RedAccent400
  | RedAccent700
  | Pink100
  | Pink200
  | Pink300
  | Pink400
  | Pink500
  | Pink600
  | Pink700
  | Pink800
  | Pink900
  | PinkAccent100
  | PinkAccent200
  | PinkAccent400
  | PinkAccent700
  | Purple100
  | Purple200
  | Purple300
  | Purple400
  | Purple500
  | Purple600
  | Purple700
  | Purple800
  | Purple900
  | PurpleAccent100
  | PurpleAccent200
  | PurpleAccent400
  | PurpleAccent700
  | DeepPurple100
  | DeepPurple200
  | DeepPurple300
  | DeepPurple400
  | DeepPurple500
  | DeepPurple600
  | DeepPurple700
  | DeepPurple800
  | DeepPurple900
  | DeepPurpleAccent100
  | DeepPurpleAccent200
  | DeepPurpleAccent400
  | DeepPurpleAccent700
  | Indigo100
  | Indigo200
  | Indigo300
  | Indigo400
  | Indigo500
  | Indigo600
  | Indigo700
  | Indigo800
  | Indigo900
  | IndigoAccent100
  | IndigoAccent200
  | IndigoAccent400
  | IndigoAccent700
  | Blue100
  | Blue200
  | Blue300
  | Blue400
  | Blue500
  | Blue600
  | Blue700
  | Blue800
  | Blue900
  | BlueAccent100
  | BlueAccent200
  | BlueAccent400
  | BlueAccent700
  | LightBlue100
  | LightBlue200
  | LightBlue300
  | LightBlue400
  | LightBlue500
  | LightBlue600
  | LightBlue700
  | LightBlue800
  | LightBlue900
  | LightBlueAccent100
  | LightBlueAccent200
  | LightBlueAccent400
  | LightBlueAccent700
  | Cyan100
  | Cyan200
  | Cyan300
  | Cyan400
  | Cyan500
  | Cyan600
  | Cyan700
  | Cyan800
  | Cyan900
  | CyanAccent100
  | CyanAccent200
  | CyanAccent400
  | CyanAccent700
  | Teal100
  | Teal200
  | Teal300
  | Teal400
  | Teal500
  | Teal600
  | Teal700
  | Teal800
  | Teal900
  | TealAccent100
  | TealAccent200
  | TealAccent400
  | TealAccent700
  | Green100
  | Green200
  | Green300
  | Green400
  | Green500
  | Green600
  | Green700
  | Green800
  | Green900
  | GreenAccent100
  | GreenAccent200
  | GreenAccent400
  | GreenAccent700
  | LightGreen100
  | LightGreen200
  | LightGreen300
  | LightGreen400
  | LightGreen500
  | LightGreen600
  | LightGreen700
  | LightGreen800
  | LightGreen900
  | LightGreenAccent100
  | LightGreenAccent200
  | LightGreenAccent400
  | LightGreenAccent700
  | Lime100
  | Lime200
  | Lime300
  | Lime400
  | Lime500
  | Lime600
  | Lime700
  | Lime800
  | Lime900
  | LimeAccent100
  | LimeAccent200
  | LimeAccent400
  | LimeAccent700
  | Yellow100
  | Yellow200
  | Yellow300
  | Yellow400
  | Yellow500
  | Yellow600
  | Yellow700
  | Yellow800
  | Yellow900
  | YellowAccent100
  | YellowAccent200
  | YellowAccent400
  | YellowAccent700
  | Amber100
  | Amber200
  | Amber300
  | Amber400
  | Amber500
  | Amber600
  | Amber700
  | Amber800
  | Amber900
  | AmberAccent100
  | AmberAccent200
  | AmberAccent400
  | AmberAccent700
  | Orange100
  | Orange200
  | Orange300
  | Orange400
  | Orange500
  | Orange600
  | Orange700
  | Orange800
  | Orange900
  | OrangeAccent100
  | OrangeAccent200
  | OrangeAccent400
  | OrangeAccent700
  | DeepOrange100
  | DeepOrange200
  | DeepOrange300
  | DeepOrange400
  | DeepOrange500
  | DeepOrange600
  | DeepOrange700
  | DeepOrange800
  | DeepOrange900
  | DeepOrangeAccent100
  | DeepOrangeAccent200
  | DeepOrangeAccent400
  | DeepOrangeAccent700
  | Brown100
  | Brown200
  | Brown300
  | Brown400
  | Brown500
  | Brown600
  | Brown700
  | Brown800
  | Brown900
  | Grey100
  | Grey200
  | Grey300
  | Grey400
  | Grey500
  | Grey600
  | Grey700
  | Grey800
  | Grey900
  | BlueGrey100
  | BlueGrey200
  | BlueGrey300
  | BlueGrey400
  | BlueGrey500
  | BlueGrey600
  | BlueGrey700
  | BlueGrey800
  | BlueGrey900
  | Black
  | White
  | PrimaryColor
  | PrimaryContrastColor
  | PrimaryDarkColor
  | AccentColor
  | AccentContrastColor
  deriving (Show, Read, Eq, Ord, Enum, Generic)

colorToText :: Color -> Text
colorToText Red100               = "red-100"
colorToText Red200               = "red-200"
colorToText Red300               = "red-300"
colorToText Red400               = "red-400"
colorToText Red500               = "red-500"
colorToText Red600               = "red-600"
colorToText Red700               = "red-700"
colorToText Red800               = "red-800"
colorToText Red900               = "red-900"
colorToText RedAccent100         = "red-A100"
colorToText RedAccent200         = "red-A200"
colorToText RedAccent400         = "red-A400"
colorToText RedAccent700         = "red-A700"
colorToText Pink100              = "pink-100"
colorToText Pink200              = "pink-200"
colorToText Pink300              = "pink-300"
colorToText Pink400              = "pink-400"
colorToText Pink500              = "pink-500"
colorToText Pink600              = "pink-600"
colorToText Pink700              = "pink-700"
colorToText Pink800              = "pink-800"
colorToText Pink900              = "pink-900"
colorToText PinkAccent100        = "pink-A100"
colorToText PinkAccent200        = "pink-A200"
colorToText PinkAccent400        = "pink-A400"
colorToText PinkAccent700        = "pink-A700"
colorToText Purple100            = "purple-100"
colorToText Purple200            = "purple-200"
colorToText Purple300            = "purple-300"
colorToText Purple400            = "purple-400"
colorToText Purple500            = "purple-500"
colorToText Purple600            = "purple-600"
colorToText Purple700            = "purple-700"
colorToText Purple800            = "purple-800"
colorToText Purple900            = "purple-900"
colorToText PurpleAccent100      = "purple-A100"
colorToText PurpleAccent200      = "purple-A200"
colorToText PurpleAccent400      = "purple-A400"
colorToText PurpleAccent700      = "purple-A700"
colorToText DeepPurple100        = "deep-purple-100"
colorToText DeepPurple200        = "deep-purple-200"
colorToText DeepPurple300        = "deep-purple-300"
colorToText DeepPurple400        = "deep-purple-400"
colorToText DeepPurple500        = "deep-purple-500"
colorToText DeepPurple600        = "deep-purple-600"
colorToText DeepPurple700        = "deep-purple-700"
colorToText DeepPurple800        = "deep-purple-800"
colorToText DeepPurple900        = "deep-purple-900"
colorToText DeepPurpleAccent100  = "deep-purple-A100"
colorToText DeepPurpleAccent200  = "deep-purple-A200"
colorToText DeepPurpleAccent400  = "deep-purple-A400"
colorToText DeepPurpleAccent700  = "deep-purple-A700"
colorToText Indigo100            = "indigo-100"
colorToText Indigo200            = "indigo-200"
colorToText Indigo300            = "indigo-300"
colorToText Indigo400            = "indigo-400"
colorToText Indigo500            = "indigo-500"
colorToText Indigo600            = "indigo-600"
colorToText Indigo700            = "indigo-700"
colorToText Indigo800            = "indigo-800"
colorToText Indigo900            = "indigo-900"
colorToText IndigoAccent100      = "indigo-A100"
colorToText IndigoAccent200      = "indigo-A200"
colorToText IndigoAccent400      = "indigo-A400"
colorToText IndigoAccent700      = "indigo-A700"
colorToText Blue100              = "blue-100"
colorToText Blue200              = "blue-200"
colorToText Blue300              = "blue-300"
colorToText Blue400              = "blue-400"
colorToText Blue500              = "blue-500"
colorToText Blue600              = "blue-600"
colorToText Blue700              = "blue-700"
colorToText Blue800              = "blue-800"
colorToText Blue900              = "blue-900"
colorToText BlueAccent100        = "blue-A100"
colorToText BlueAccent200        = "blue-A200"
colorToText BlueAccent400        = "blue-A400"
colorToText BlueAccent700        = "blue-A700"
colorToText LightBlue100         = "light-blue-100"
colorToText LightBlue200         = "light-blue-200"
colorToText LightBlue300         = "light-blue-300"
colorToText LightBlue400         = "light-blue-400"
colorToText LightBlue500         = "light-blue-500"
colorToText LightBlue600         = "light-blue-600"
colorToText LightBlue700         = "light-blue-700"
colorToText LightBlue800         = "light-blue-800"
colorToText LightBlue900         = "light-blue-900"
colorToText LightBlueAccent100   = "light-blue-A100"
colorToText LightBlueAccent200   = "light-blue-A200"
colorToText LightBlueAccent400   = "light-blue-A400"
colorToText LightBlueAccent700   = "light-blue-A700"
colorToText Cyan100              = "cyan-100"
colorToText Cyan200              = "cyan-200"
colorToText Cyan300              = "cyan-300"
colorToText Cyan400              = "cyan-400"
colorToText Cyan500              = "cyan-500"
colorToText Cyan600              = "cyan-600"
colorToText Cyan700              = "cyan-700"
colorToText Cyan800              = "cyan-800"
colorToText Cyan900              = "cyan-900"
colorToText CyanAccent100        = "cyan-A100"
colorToText CyanAccent200        = "cyan-A200"
colorToText CyanAccent400        = "cyan-A400"
colorToText CyanAccent700        = "cyan-A700"
colorToText Teal100              = "teal-100"
colorToText Teal200              = "teal-200"
colorToText Teal300              = "teal-300"
colorToText Teal400              = "teal-400"
colorToText Teal500              = "teal-500"
colorToText Teal600              = "teal-600"
colorToText Teal700              = "teal-700"
colorToText Teal800              = "teal-800"
colorToText Teal900              = "teal-900"
colorToText TealAccent100        = "teal-A100"
colorToText TealAccent200        = "teal-A200"
colorToText TealAccent400        = "teal-A400"
colorToText TealAccent700        = "teal-A700"
colorToText Green100             = "green-100"
colorToText Green200             = "green-200"
colorToText Green300             = "green-300"
colorToText Green400             = "green-400"
colorToText Green500             = "green-500"
colorToText Green600             = "green-600"
colorToText Green700             = "green-700"
colorToText Green800             = "green-800"
colorToText Green900             = "green-900"
colorToText GreenAccent100       = "green-A100"
colorToText GreenAccent200       = "green-A200"
colorToText GreenAccent400       = "green-A400"
colorToText GreenAccent700       = "green-A700"
colorToText LightGreen100        = "light-green-100"
colorToText LightGreen200        = "light-green-200"
colorToText LightGreen300        = "light-green-300"
colorToText LightGreen400        = "light-green-400"
colorToText LightGreen500        = "light-green-500"
colorToText LightGreen600        = "light-green-600"
colorToText LightGreen700        = "light-green-700"
colorToText LightGreen800        = "light-green-800"
colorToText LightGreen900        = "light-green-900"
colorToText LightGreenAccent100  = "light-green-A100"
colorToText LightGreenAccent200  = "light-green-A200"
colorToText LightGreenAccent400  = "light-green-A400"
colorToText LightGreenAccent700  = "light-green-A700"
colorToText Lime100              = "lime-100"
colorToText Lime200              = "lime-200"
colorToText Lime300              = "lime-300"
colorToText Lime400              = "lime-400"
colorToText Lime500              = "lime-500"
colorToText Lime600              = "lime-600"
colorToText Lime700              = "lime-700"
colorToText Lime800              = "lime-800"
colorToText Lime900              = "lime-900"
colorToText LimeAccent100        = "lime-A100"
colorToText LimeAccent200        = "lime-A200"
colorToText LimeAccent400        = "lime-A400"
colorToText LimeAccent700        = "lime-A700"
colorToText Yellow100            = "yellow-100"
colorToText Yellow200            = "yellow-200"
colorToText Yellow300            = "yellow-300"
colorToText Yellow400            = "yellow-400"
colorToText Yellow500            = "yellow-500"
colorToText Yellow600            = "yellow-600"
colorToText Yellow700            = "yellow-700"
colorToText Yellow800            = "yellow-800"
colorToText Yellow900            = "yellow-900"
colorToText YellowAccent100      = "yellow-A100"
colorToText YellowAccent200      = "yellow-A200"
colorToText YellowAccent400      = "yellow-A400"
colorToText YellowAccent700      = "yellow-A700"
colorToText Amber100             = "amber-100"
colorToText Amber200             = "amber-200"
colorToText Amber300             = "amber-300"
colorToText Amber400             = "amber-400"
colorToText Amber500             = "amber-500"
colorToText Amber600             = "amber-600"
colorToText Amber700             = "amber-700"
colorToText Amber800             = "amber-800"
colorToText Amber900             = "amber-900"
colorToText AmberAccent100       = "amber-A100"
colorToText AmberAccent200       = "amber-A200"
colorToText AmberAccent400       = "amber-A400"
colorToText AmberAccent700       = "amber-A700"
colorToText Orange100            = "orange-100"
colorToText Orange200            = "orange-200"
colorToText Orange300            = "orange-300"
colorToText Orange400            = "orange-400"
colorToText Orange500            = "orange-500"
colorToText Orange600            = "orange-600"
colorToText Orange700            = "orange-700"
colorToText Orange800            = "orange-800"
colorToText Orange900            = "orange-900"
colorToText OrangeAccent100      = "orange-A100"
colorToText OrangeAccent200      = "orange-A200"
colorToText OrangeAccent400      = "orange-A400"
colorToText OrangeAccent700      = "orange-A700"
colorToText DeepOrange100        = "deep-orange-100"
colorToText DeepOrange200        = "deep-orange-200"
colorToText DeepOrange300        = "deep-orange-300"
colorToText DeepOrange400        = "deep-orange-400"
colorToText DeepOrange500        = "deep-orange-500"
colorToText DeepOrange600        = "deep-orange-600"
colorToText DeepOrange700        = "deep-orange-700"
colorToText DeepOrange800        = "deep-orange-800"
colorToText DeepOrange900        = "deep-orange-900"
colorToText DeepOrangeAccent100  = "deep-orange-A100"
colorToText DeepOrangeAccent200  = "deep-orange-A200"
colorToText DeepOrangeAccent400  = "deep-orange-A400"
colorToText DeepOrangeAccent700  = "deep-orange-A700"
colorToText Brown100             = "brown-100"
colorToText Brown200             = "brown-200"
colorToText Brown300             = "brown-300"
colorToText Brown400             = "brown-400"
colorToText Brown500             = "brown-500"
colorToText Brown600             = "brown-600"
colorToText Brown700             = "brown-700"
colorToText Brown800             = "brown-800"
colorToText Brown900             = "brown-900"
colorToText Grey100              = "grey-100"
colorToText Grey200              = "grey-200"
colorToText Grey300              = "grey-300"
colorToText Grey400              = "grey-400"
colorToText Grey500              = "grey-500"
colorToText Grey600              = "grey-600"
colorToText Grey700              = "grey-700"
colorToText Grey800              = "grey-800"
colorToText Grey900              = "grey-900"
colorToText BlueGrey100          = "blue-grey-100"
colorToText BlueGrey200          = "blue-grey-200"
colorToText BlueGrey300          = "blue-grey-300"
colorToText BlueGrey400          = "blue-grey-400"
colorToText BlueGrey500          = "blue-grey-500"
colorToText BlueGrey600          = "blue-grey-600"
colorToText BlueGrey700          = "blue-grey-700"
colorToText BlueGrey800          = "blue-grey-800"
colorToText BlueGrey900          = "blue-grey-900"
colorToText Black                = "black"
colorToText White                = "white"
colorToText PrimaryColor         = "primary"
colorToText PrimaryContrastColor = "primary-contrast"
colorToText PrimaryDarkColor     = "primary-dark"
colorToText AccentColor          = "accent"
colorToText AccentContrastColor  = "accent-contrast"

instance HtmlClass Color where
  toHtmlClass c = " mdl-color--" <> colorToText c <> " "

newtype TextColor =
  TextColor Color
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving newtype Enum

spacer_ :: Applicative m => Lucid.HtmlT m ()
spacer_ = Lucid.div_ [Lucid.class_ "mdl-layout-spacer"] mempty

instance HtmlClass TextColor where
  toHtmlClass (TextColor c) = " mdl-color-text--" <> colorToText c <> " "

class HtmlClass a where
  toHtmlClass :: a -> Text

instance HtmlClass a => HtmlClass (Maybe a) where
  toHtmlClass Nothing  = ""
  toHtmlClass (Just a) = toHtmlClass a
