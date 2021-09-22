{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

import qualified Lucid.Mdl.Base      as Base
import qualified Lucid.Mdl.Button    as Button
import qualified Lucid.Mdl.Chip      as Chip
import qualified Lucid.Mdl.Card as Card
import qualified Lucid.Mdl.Head      as Head
import qualified Lucid.Mdl.Slider    as Slider
import qualified Lucid.Mdl.TextField as TextField
import qualified Lucid.Mdl.Toggle    as Toggle

import           Data.Default.Class (Default (def))
import qualified Data.Text.Lazy.IO  as T
import qualified Lucid              as L

main :: IO ()
main = T.putStrLn $ L.renderText html

html :: L.Html ()
html =
  L.html_ $ do
    L.head_ $ do
      Head.script_
      Head.css_ Head.Pink Head.Indigo
      Head.font_
      L.style_ "body { padding: 20px; box-sizing: border-box; }"
    L.body_ $ do
      L.h1_ "lucid-mdl"
      L.h2_ "Lucid.Mdl.Toggle"

      L.h3_ "Button"
      L.div_ $ do
        Button.button_ def "Button"
        space
        Button.button_ def { Button.style = Just Button.Raised, Button.color = Just Button.Colored, Button.ripple = Just Base.Ripple } "Button"

      L.h3_ "Checkbox"
      L.div_ $ do
        Toggle.checkbox_ def { Toggle.ripple = Just Base.Ripple, Toggle.attributes = [L.style_ "display: block;"] } "checkbox1" "Checkbox 1"
        Toggle.checkbox_ def { Toggle.ripple = Just Base.Ripple, Toggle.attributes = [L.style_ "display: block;"] } "checkbox2" "Checkbox 2"

      L.h3_ "Radio button"
      L.div_ $ do
        Toggle.radio_ def { Toggle.ripple = Just Base.Ripple, Toggle.attributes = [L.style_ "display: block;"], Toggle.inputAttributes = [L.checked_] } "radio1" "radio-group" "Radio button 1"
        Toggle.radio_ def { Toggle.ripple = Just Base.Ripple, Toggle.attributes = [L.style_ "display: block;"] } "radio2" "radio-group" "Radio button 2"

      L.h3_ "Icon toggle"
      L.div_ $ do
        Toggle.iconToggle_ def { Toggle.ripple = Just Base.Ripple } "thumb-up" "thumb_up"
        Toggle.iconToggle_ def { Toggle.ripple = Just Base.Ripple } "thumb-down" "thumb_down"

      L.h3_ "Switch"
      L.div_ $ do
        Toggle.switch_ def { Toggle.ripple = Just Base.Ripple } "switch1" ""

      L.h3_ "Slider"
      L.div_ $ do
        L.div_ [L.style_ "display: block; width: 400px;"] $ do
          Slider.slider_ def { Slider.attributes = [L.min_ "0", L.max_ "100", L.value_ "0", L.tabindex_ "0"] }

      L.h3_ "Chip"
      L.div_ $ do
        Chip.chip_ def "Chip"
        space
        Chip.chip_ def { Chip.deletable = Just def } "Deletable"
        space
        Chip.button_ def "Button"
        space
        Chip.textContact_ def { Chip.contactAttributes = [L.classes_ [Base.toHtmlClass Base.PrimaryColor, Base.toHtmlClass $ Base.TextColor Base.White]] } "A" "Contact"
        space
        Chip.imageContact_ def "https://getmdl.io/templates/dashboard/images/user.jpg" "Contact"

      L.h3_ "Text field"
      L.div_ $
        L.form_ $ do
          L.div_ $ TextField.textField_ def "text-field-1" "Text field"
          L.div_ $ TextField.textField_ def { TextField.error = Just (TextField.ErrorConfig "-?[0-9]*(\\.[0-9]+)?" [] "Input must be a number") } "text-field-2" "Text field"
          L.div_ $ TextField.textField_ def { TextField.floatingLabel = Just TextField.FloatingLabel } "text-field-3" "Text field"
          L.div_ $ TextField.textArea_ def "text-area-1" "Text area" ""

      L.h3_ "Card"
      L.div_ $ do
        Card.card_ def { Card.cardAttributes = [L.class_ "mdl-shadow--2dp"] } (L.h4_ "Card")

space :: L.Html ()
space = L.span_ [L.style_ "display: inline-block; width: 8px;"] ""
