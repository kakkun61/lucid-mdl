{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

import qualified Lucid.MaterialDesign.Icon           as Icon
import qualified Lucid.MaterialDesign.Lite.Base      as Base
import qualified Lucid.MaterialDesign.Lite.Button    as Button
import qualified Lucid.MaterialDesign.Lite.Card      as Card
import qualified Lucid.MaterialDesign.Lite.Chip      as Chip
import qualified Lucid.MaterialDesign.Lite.Head      as Head
import qualified Lucid.MaterialDesign.Lite.Slider    as Slider
import qualified Lucid.MaterialDesign.Lite.TextField as TextField
import qualified Lucid.MaterialDesign.Lite.Toggle    as Toggle

import           Data.Default.Class (Default (def))
import           Data.String        (IsString)
import qualified Data.Text.Lazy.IO  as T
import qualified Lucid              as L
import qualified Lucid.MaterialDesign.Lite.Dialog as Dialog

main :: IO ()
main = T.putStrLn $ L.renderText html

html :: L.Html ()
html =
  L.html_ $ do
    L.head_ $ do
      Head.script_
      Head.css_ Head.Pink Head.Indigo
      Icon.head_
      L.style_ "body { padding: 20px; box-sizing: border-box; }"
    L.body_ $ do
      L.h1_ "lucid-mdl"

      L.h3_ "Button"
      L.div_ $ do
        Button.button_ def "Button"
        space
        Button.button_ def { Button.style = Just Button.Raised, Button.color = Just Button.Colored, Button.ripple = Base.Ripple True } "Button"

      L.h3_ "Checkbox"
      L.div_ $ do
        Toggle.checkbox_ def { Toggle.ripple = Base.Ripple True, Toggle.attributes = [L.style_ "display: block;"] } "checkbox1" "Checkbox 1"
        Toggle.checkbox_ def { Toggle.ripple = Base.Ripple True, Toggle.attributes = [L.style_ "display: block;"] } "checkbox2" "Checkbox 2"

      L.h3_ "Radio button"
      L.div_ $ do
        Toggle.radio_ def { Toggle.ripple = Base.Ripple True, Toggle.attributes = [L.style_ "display: block;"], Toggle.inputAttributes = [L.checked_] } "radio1" "radio-group" "Radio button 1"
        Toggle.radio_ def { Toggle.ripple = Base.Ripple True, Toggle.attributes = [L.style_ "display: block;"] } "radio2" "radio-group" "Radio button 2"

      L.h3_ "Icon toggle"
      L.div_ $ do
        Toggle.iconToggle_ def { Toggle.ripple = Base.Ripple True } "thumb-up" $ Icon.icon_ "thumb_up"
        Toggle.iconToggle_ def { Toggle.ripple = Base.Ripple True } "thumb-down" $ Icon.icon_ "thumb_down"

      L.h3_ "Switch"
      L.div_ $ do
        Toggle.switch_ def { Toggle.ripple = Base.Ripple True } "switch1" ""

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
          L.div_ $ TextField.textField_ def { TextField.floatingLabel = TextField.FloatingLabel True } "text-field-3" "Text field"
          L.div_ $ TextField.textArea_ def "text-area-1" "Text area" ""

      L.h3_ "Card"
      L.div_ $ do
        Card.card_ def { Card.attributes = [L.class_ "mdl-shadow--2dp"] } $ do
          Card.title_ def $ do
            Card.titleText_ $ L.h4_ "Card"
            Card.subtitleText_ $ L.h5_ "Material"
          Card.supportingText_ def loremIpsum
          Card.actions_ def { Card.border = Card.Border True } $ Button.button_ def { Button.color = Just Button.Colored } "Button"
          Card.menu_ def $ Button.button_ def { Button.style = Just Button.Icon } $ Icon.icon_ "share"

      L.h3_ "Dialog"
      L.div_ $ do
        Button.button_ def { Button.attributes = [L.id_ "button-open-dialog"] } "Open dialog"
        Dialog.dialog_ def { Dialog.attributes = [L.id_ "dialog"] } $ do
          Dialog.title_ $ L.h4_ "Dialog"
          Dialog.content_ def $ L.p_ loremIpsum
          Dialog.actions_ def $ do
            Button.button_ def { Button.attributes = [L.id_ "button-close-dialog"] } "Close dialog"
        L.script_
          "var dialog = document.getElementById('dialog');\
          \var showDialogButton = document.getElementById('button-open-dialog');\
          \if (! dialog.showModal) {\
          \  dialogPolyfill.registerDialog(dialog);\
          \}\
          \showDialogButton.addEventListener('click', function() {\
          \  dialog.showModal();\
          \});\
          \document.getElementById('button-close-dialog').addEventListener('click', function() {\
          \  dialog.close();\
          \});"

space :: L.Html ()
space = L.span_ [L.style_ "display: inline-block; width: 8px;"] ""

loremIpsum :: IsString a => a
loremIpsum =
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation \
  \ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint \
  \occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
